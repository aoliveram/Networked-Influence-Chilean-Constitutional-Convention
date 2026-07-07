"""
0b-audit-profiles-dual-source.py
Auditoría dual-fuente (BCN + Wikipedia) de los perfiles de los 154 convencionales (P5)
e imputación automática de covariables.

Para cada convencional, gemini-3.5-flash extrae las características desde (a) el texto
BCN ya scrapeado y (b) su artículo de Wikipedia (vía search grounding), de forma
independiente y referidas AL MOMENTO de ser convencional (jul-2021 a jul-2022).
Corrida v2: lotes de 6 convencionales; grado académico en escala 0-3
(0 sin estudios universitarios terminados, 1 educación superior terminada,
2 magíster/máster, 3 doctorado). La corrida v1 (lotes de 5, grado 0-2) queda en
gemini_batches/ como referencia de consistencia.

Imputación (regla del usuario, 2026-07-07): BCN es ground truth; si BCN no informa,
Wikipedia; si ninguna informa, se conserva el valor base. Las validaciones manuales
de manual_validations.json SIEMPRE prevalecen sobre la regla automática.

Outputs (data/raw/profile-audit/):
  profiles-batches/batch_NN.json  respuesta cruda por lote (corrida v2)
  profile_audit_values.csv        valores por fuente (154 x campos x {bcn, wiki})
  profile_audit_table.csv         tabla 154 x características con símbolos:
                                    =  ambas fuentes coinciden
                                    ≠  ambas fuentes discrepan
                                    B  solo BCN informa
                                    W  solo Wikipedia informa
                                    ∅  ninguna fuente informa
  discrepancias_pipeline.csv      diferencias auditoría vs conventional-profiles.json de CPT
  runs_consistency.csv            desacuerdos entre corrida v1 y v2 (por fuente y campo)
  imputation_report.csv           cambios aplicados al regenerar los perfiles
Y regenera data/raw/conventional-profiles.json (154 perfiles imputados).

Uso:
  python3 code/0b-audit-profiles-dual-source.py            # lotes pendientes + tablas + consistencia + imputación
  python3 code/0b-audit-profiles-dual-source.py --test     # lote de prueba con 6 casos difíciles
  python3 code/0b-audit-profiles-dual-source.py --tables   # solo reconstruye tablas del caché
  python3 code/0b-audit-profiles-dual-source.py --impute   # solo consistencia + imputación (sin API)
"""

import json
import os
import re
import csv
import sys
import time
import unicodedata
from collections import Counter

import requests

REPO_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CPT_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - constitutional-proposal-tracking"
RAW_PROFILES = os.path.join(CPT_DIR, "conventionals-bcn-webscrapping/conventional-profiles-raw.json")
PROC_PROFILES = os.path.join(CPT_DIR, "conventionals-bcn-webscrapping/conventional-profiles.json")
MEMBERS = os.path.join(REPO_DIR, "data/raw/convention_members.json")
OUT_DIR = os.path.join(REPO_DIR, "data/raw/profile-audit")
BATCH_DIR = os.path.join(OUT_DIR, "profiles-batches")      # corrida v2: grado 0-3, lotes de 6
BATCH_DIR_V1 = os.path.join(OUT_DIR, "gemini_batches")     # corrida v1: grado 0-2, lotes de 5
MANUAL_VALIDATIONS = os.path.join(OUT_DIR, "manual_validations.json")
PROFILES_OUT = os.path.join(REPO_DIR, "data/raw/conventional-profiles.json")

MODEL = "gemini-3.5-flash"
API_URL = f"https://generativelanguage.googleapis.com/v1beta/models/{MODEL}:generateContent"
BATCH_SIZE = 6
FIELDS = ["genero", "fecha_nacimiento", "afiliacion_politica", "lista_electoral",
          "distrito", "profesion", "es_abogado", "grado_academico_nivel",
          "experiencia_previa_institucional", "experiencia_detalle"]
# campos comparados en la tabla de símbolos (profesion se compara vía es_abogado;
# experiencia_detalle es texto libre de apoyo)
SYMBOL_FIELDS = ["genero", "fecha_nacimiento", "afiliacion_politica", "lista_electoral",
                 "distrito", "es_abogado", "grado_academico_nivel",
                 "experiencia_previa_institucional"]
# mapeo de la escala de grado v1 (0-2) a la v2 (0-3) para el chequeo de consistencia
GRADO_V1_TO_V2 = {0: {0, 1}, 1: {2}, 2: {3}}

PROMPT_TEMPLATE = """Audita los perfiles de estos {n} convencionales constituyentes de la Convención Constitucional de Chile (2021-2022).

Para cada convencional completa DOS fuentes de forma INDEPENDIENTE (no copies datos de una fuente a la otra):
- "bcn": usando EXCLUSIVAMENTE el texto BCN adjunto de esa persona.
- "wikipedia": buscando su artículo en Wikipedia en español (es.wikipedia.org). Si no tiene artículo propio, deja todos sus campos en null y "url" en null.

REGLA CENTRAL: reporta la situación AL MOMENTO de ser convencional (julio 2021 - julio 2022): afiliación política vigente en ese período (si militó antes pero asumió como independiente, usa "Independiente"; si renunció al partido DESPUÉS de la Convención, usa el partido de entonces); cargos públicos ANTERIORES al 04-07-2021.

Campos por fuente (null si la fuente no informa el dato; no infieras):
- genero: "F" o "M"
- fecha_nacimiento: "YYYY-MM-DD" (o "YYYY" si solo hay año)
- afiliacion_politica: nombre del partido, o "Independiente"
- lista_electoral: lista o coalición por la que fue electo/a (p. ej. "Vamos por Chile", "Lista del Apruebo", "Apruebo Dignidad", "La Lista del Pueblo", "Independientes No Neutrales", "Escaños reservados")
- distrito: "D<numero>" (p. ej. "D12"), o "Escaño reservado: <pueblo>"
- profesion: string breve
- es_abogado: true/false
- grado_academico_nivel: 3 = doctorado; 2 = magíster o máster (no cuenta diplomado); 1 = educación superior terminada (título profesional o licenciatura); 0 = sin estudios universitarios terminados
- experiencia_previa_institucional: true si ANTES de julio 2021 ocupó cargos públicos (diputado/a, senador/a, alcalde/sa, concejal/a, consejero/a regional, seremi, intendente/a, ministro/a, subsecretario/a, embajador/a, jefatura de servicio público); false si no
- experiencia_detalle: cargos con años, o null

Responde SOLO con un array JSON válido de {n} objetos, sin texto adicional:
[{{"nombre": "<idéntico al input>", "bcn": {{...}}, "wikipedia": {{..., "url": "..."}}}}]

CONVENCIONALES:
{people}"""


def norm(s):
    if s is None:
        return None
    s = unicodedata.normalize("NFKD", str(s)).encode("ascii", "ignore").decode()
    return re.sub(r"\s+", " ", s).strip().lower()


def truncate(s, n):
    s = s or ""
    return s[:n]


def load_inputs():
    members = json.load(open(MEMBERS, encoding="utf-8"))
    raw = {p["nombre_original_json"]: p for p in json.load(open(RAW_PROFILES, encoding="utf-8"))}
    return members, raw


def bcn_block(name, raw):
    p = raw.get(name)
    if not p:
        return f"### {name}\n(sin texto BCN)"
    parts = [
        f"### {name}",
        f"nombre_completo: {p.get('nombre_completo')}",
        f"fecha_nacimiento: {p.get('fecha_nacimiento')}",
        f"distrito: {p.get('distrito')}",
        f"afiliacion_politica: {p.get('afiliacion_politica')}",
        f"profesion: {p.get('profesion')}",
        f"grado_academico: {p.get('grado_academico')}",
        f"intro: {truncate(p.get('intro_wiki'), 2500)}",
        f"estudios_y_vida_laboral: {truncate(p.get('estudios_y_vida_laboral'), 3000)}",
        f"trayectoria_politica_y_publica: {truncate(p.get('trayectoria_politica_y_publica'), 3500)}",
    ]
    return "\n".join(parts)


def call_gemini(batch_names, raw, retries=3):
    people = "\n\n".join(bcn_block(n, raw) for n in batch_names)
    prompt = PROMPT_TEMPLATE.format(n=len(batch_names), people=people)
    body = {
        "contents": [{"role": "user", "parts": [{"text": prompt}]}],
        "tools": [{"google_search": {}}],
        "generationConfig": {"temperature": 0.1, "maxOutputTokens": 16384},
    }
    last_err = None
    for attempt in range(retries):
        try:
            r = requests.post(
                API_URL,
                params={"key": os.environ["GOOGLE_API_KEY"]},
                json=body,
                timeout=300,
            )
            r.raise_for_status()
            resp = r.json()
            text = "".join(part.get("text", "")
                           for part in resp["candidates"][0]["content"]["parts"])
            text = re.sub(r"^```(json)?\s*|\s*```$", "", text.strip())
            data = json.loads(text)
            assert isinstance(data, list) and len(data) == len(batch_names), \
                f"esperaba {len(batch_names)} objetos, llegaron {len(data) if isinstance(data, list) else type(data)}"
            return data
        except Exception as e:
            key = os.environ.get("GOOGLE_API_KEY", "")
            err = str(e).replace(key, "<API_KEY>") if key else str(e)
            last_err = err
            wait = 15 * (attempt + 1)
            print(f"    intento {attempt + 1} falló ({err}); reintento en {wait}s")
            time.sleep(wait)
    raise RuntimeError(f"lote falló tras {retries} intentos: {last_err}")


def run_batches(members, raw, test=False):
    os.makedirs(BATCH_DIR, exist_ok=True)
    if test:
        batches = [(999, ["Zuniga, Luis Arturo", "Tepper, Maria Angelica", "Garin, Renato",
                          "Chahin, Fuad", "Gonzalez, Dayana", "Atria, Fernando"])]
    else:
        batches = [(i, members[i * BATCH_SIZE:(i + 1) * BATCH_SIZE])
                   for i in range((len(members) + BATCH_SIZE - 1) // BATCH_SIZE)]
    for idx, names in batches:
        path = os.path.join(BATCH_DIR, f"batch_{idx:02d}.json")
        if os.path.exists(path):
            print(f"  lote {idx:02d} ya existe, salto")
            continue
        print(f"  lote {idx:02d}: {names[0]} ... {names[-1]}")
        data = call_gemini(names, raw)
        returned = [d.get("nombre") for d in data]
        if returned != names:
            print(f"    AVISO: nombres devueltos no calzan 1:1 con el input: {returned}")
        json.dump(data, open(path, "w", encoding="utf-8"), ensure_ascii=False, indent=2)
        time.sleep(2)


# --------------------------- comparación y tablas ---------------------------

PARTY_ALIASES = {
    "rn": "renovacion nacional", "udi": "union democrata independiente",
    "ps": "partido socialista", "pc": "partido comunista",
    "pcch": "partido comunista", "dc": "democracia cristiana",
    "pdc": "democracia cristiana", "partido democrata cristiano": "democracia cristiana",
    "evopoli": "evolucion politica", "rd": "revolucion democratica",
    "cs": "convergencia social", "pl": "partido liberal",
    "prsd": "partido radical", "frvs": "federacion regionalista verde social",
    "pro": "partido progresista", "pri": "partido regionalista independiente",
    "ppd": "partido por la democracia",
}


def norm_party(v):
    v = norm(v)
    if v is None:
        return None
    v = re.sub(r"\bde chile\b", "", v).strip()
    v = re.sub(r"^partido\s+(?=(comunista|socialista|liberal|radical|progresista|republicano))", "", v).strip()
    v = PARTY_ALIASES.get(v, v)
    return v


def norm_district(v):
    v = norm(v)
    if v is None:
        return None
    if "escano" in v or "reservado" in v or "pueblo" in v:
        return "escano:" + re.sub(r".*(escano reservado:?|pueblo)\s*", "", v).strip()
    m = re.search(r"\d+", v)
    return f"d{m.group()}" if m else v


def norm_list(v):
    v = norm(v)
    if v is None:
        return None
    return re.sub(r"^(la )?lista (de |del )?", "", v).strip()


def norm_date(v):
    v = norm(v)
    if v is None:
        return None
    m = re.match(r"(\d{4})(-\d{2}-\d{2})?", v)
    return m.groups() if m else (v, None)


def compare_field(field, b, w):
    """Devuelve símbolo de concordancia entre valor BCN y valor Wikipedia."""
    if b is None and w is None:
        return "∅"
    if w is None:
        return "B"
    if b is None:
        return "W"
    if field == "fecha_nacimiento":
        yb, fb = norm_date(b)
        yw, fw = norm_date(w)
        if fb and fw:
            return "=" if (yb, fb) == (yw, fw) else "≠"
        return "=" if yb == yw else "≠"
    if field == "afiliacion_politica":
        return "=" if norm_party(b) == norm_party(w) else "≠"
    if field == "distrito":
        return "=" if norm_district(b) == norm_district(w) else "≠"
    if field == "lista_electoral":
        nb, nw = norm_list(b), norm_list(w)
        return "=" if (nb == nw or nb in nw or nw in nb) else "≠"
    if field in ("es_abogado", "experiencia_previa_institucional"):
        return "=" if bool(b) == bool(w) else "≠"
    if field == "grado_academico_nivel":
        return "=" if int(b) == int(w) else "≠"
    return "=" if norm(b) == norm(w) else "≠"


def age_at(date_str, ref=(2021, 7, 4)):
    y, full = norm_date(date_str) if date_str else (None, None)
    if not y:
        return None
    if full:
        m, d = int(full[1:3]), int(full[4:6])
        return ref[0] - int(y) - ((ref[1], ref[2]) < (m, d))
    return ref[0] - int(y)  # aproximada si solo hay año


def load_audited(batch_dir):
    audited = {}
    if not os.path.isdir(batch_dir):
        return audited
    for f in sorted(os.listdir(batch_dir)):
        if f.startswith("batch_") and not f.startswith("batch_999") and f.endswith(".json"):
            for rec in json.load(open(os.path.join(batch_dir, f), encoding="utf-8")):
                audited[rec["nombre"]] = rec
    return audited


def build_tables(members):
    audited = load_audited(BATCH_DIR)

    missing = [m for m in members if m not in audited]
    if missing:
        print(f"  AVISO: {len(missing)} convencionales sin auditar aún: {missing[:5]}...")

    # valores por fuente
    with open(os.path.join(OUT_DIR, "profile_audit_values.csv"), "w", newline="", encoding="utf-8") as fh:
        wr = csv.writer(fh)
        wr.writerow(["nombre"] + [f"{f}_{s}" for f in FIELDS for s in ("bcn", "wiki")] + ["wiki_url"])
        for m in members:
            rec = audited.get(m, {})
            b, w = rec.get("bcn") or {}, rec.get("wikipedia") or {}
            wr.writerow([m] + [x for f in FIELDS for x in (b.get(f), w.get(f))] + [w.get("url")])

    # tabla de símbolos 154 x características
    with open(os.path.join(OUT_DIR, "profile_audit_table.csv"), "w", newline="", encoding="utf-8") as fh:
        wr = csv.writer(fh)
        wr.writerow(["nombre"] + SYMBOL_FIELDS)
        for m in members:
            rec = audited.get(m, {})
            b, w = rec.get("bcn") or {}, rec.get("wikipedia") or {}
            wr.writerow([m] + [compare_field(f, b.get(f), w.get(f)) for f in SYMBOL_FIELDS])

    # discrepancias contra el pipeline de CPT (salida cruda del scraper)
    proc = {p["nombre_armonizado"]: p for p in json.load(open(PROC_PROFILES, encoding="utf-8"))}
    rows = []
    for m in members:
        rec = audited.get(m)
        pl = proc.get(m)
        if not rec or not pl:
            continue
        b, w = rec.get("bcn") or {}, rec.get("wikipedia") or {}

        def resolved(f):
            return b.get(f) if b.get(f) is not None else w.get(f)

        checks = [
            ("es_mujer", pl["es_mujer"], {"F": 1, "M": 0}.get(resolved("genero"))),
            ("afiliacion_agrupada", pl["afiliacion_agrupada"], resolved("afiliacion_politica")),
            ("distrito", pl["distrito"], resolved("distrito")),
            ("es_abogado", pl["es_abogado"],
             None if resolved("es_abogado") is None else int(bool(resolved("es_abogado")))),
            ("edad_al_asumir", pl["edad_al_asumir"], age_at(resolved("fecha_nacimiento"))),
            ("grado_academico_nivel", pl["grado_academico_nivel"], resolved("grado_academico_nivel")),
            ("experiencia_previa_institucional", pl["experiencia_previa_institucional"],
             None if resolved("experiencia_previa_institucional") is None
             else int(bool(resolved("experiencia_previa_institucional")))),
        ]
        for campo, v_pipe, v_audit in checks:
            if v_audit is None:
                continue
            equal = (norm_party(v_pipe) == norm_party(v_audit)) if campo == "afiliacion_agrupada" \
                else (norm_district(v_pipe) == norm_district(v_audit)) if campo == "distrito" \
                else (str(v_pipe) == str(v_audit))
            if not equal:
                rows.append([m, campo, v_pipe, v_audit,
                             b.get(campo_map(campo)), w.get(campo_map(campo))])
    with open(os.path.join(OUT_DIR, "discrepancias_pipeline.csv"), "w", newline="", encoding="utf-8") as fh:
        wr = csv.writer(fh)
        wr.writerow(["nombre", "campo", "valor_pipeline", "valor_auditoria", "valor_bcn", "valor_wiki"])
        wr.writerows(rows)
    print(f"  tablas escritas en {OUT_DIR} ({len(audited)} auditados, "
          f"{len(rows)} discrepancias con el pipeline de CPT)")


def campo_map(campo):
    return {"es_mujer": "genero", "afiliacion_agrupada": "afiliacion_politica",
            "edad_al_asumir": "fecha_nacimiento"}.get(campo, campo)


# ------------------- consistencia entre corridas v1 y v2 -------------------

def compare_runs(members):
    v1, v2 = load_audited(BATCH_DIR_V1), load_audited(BATCH_DIR)
    common = [m for m in members if m in v1 and m in v2]
    stats = {f: Counter() for f in SYMBOL_FIELDS}
    rows = []
    for m in common:
        for src in ("bcn", "wikipedia"):
            a = v1[m].get(src) or {}
            b = v2[m].get(src) or {}
            for f in SYMBOL_FIELDS:
                va, vb = a.get(f), b.get(f)
                if va is None and vb is None:
                    continue
                if va is None or vb is None:
                    stats[f]["solo_una_corrida"] += 1
                    rows.append([m, src, f, va, vb, "solo_una_corrida"])
                    continue
                if f == "grado_academico_nivel":
                    ok = int(vb) in GRADO_V1_TO_V2.get(int(va), set())
                else:
                    ok = compare_field(f, va, vb) == "="
                stats[f]["igual" if ok else "distinto"] += 1
                if not ok:
                    rows.append([m, src, f, va, vb, "distinto"])
    with open(os.path.join(OUT_DIR, "runs_consistency.csv"), "w", newline="", encoding="utf-8") as fh:
        wr = csv.writer(fh)
        wr.writerow(["nombre", "fuente", "campo", "valor_v1", "valor_v2", "estado"])
        wr.writerows(rows)
    print(f"  consistencia v1 vs v2 ({len(common)} comunes):")
    for f in SYMBOL_FIELDS:
        c = stats[f]
        tot = sum(c.values())
        pct = 100 * c["igual"] / tot if tot else 0
        print(f"    {f:36s} igual={c['igual']:4d} distinto={c['distinto']:3d} "
              f"solo_una={c['solo_una_corrida']:3d}  ({pct:.1f}% igual)")


# ------------------------------- imputación --------------------------------

def harmonize_district(base_d, audit_d):
    if audit_d is None:
        return base_d if base_d else "Desconocido"
    na = norm_district(audit_d)
    if na and na.startswith("escano:"):
        raw = re.sub(r"(?i).*escaño reservado:?\s*", "", str(audit_d)).strip()
        raw = re.sub(r"(?i)^pueblo\s+", "", raw)
        return f"Escaño reservado: {raw}"
    if base_d and norm_district(base_d) == na:
        return base_d  # mismo distrito: conservar el formato existente
    return str(audit_d)


def impute_profiles(members):
    """Regenera data/raw/conventional-profiles.json (154 perfiles): BCN como ground
    truth, Wikipedia si BCN no informa, base si ninguna informa; las validaciones
    manuales siempre prevalecen."""
    audited = load_audited(BATCH_DIR)
    base_cpt = {p["nombre_armonizado"]: p for p in json.load(open(PROC_PROFILES, encoding="utf-8"))}
    try:
        base_user = {p["nombre_armonizado"]: p for p in json.load(open(PROFILES_OUT, encoding="utf-8"))}
    except FileNotFoundError:
        base_user = {}
    manual = {}
    if os.path.exists(MANUAL_VALIDATIONS):
        for v in json.load(open(MANUAL_VALIDATIONS, encoding="utf-8"))["validaciones"]:
            manual[(v["nombre"], v["campo"])] = v["valor"]

    out, report = [], []
    for m in members:
        base = base_user.get(m) or base_cpt.get(m) or {}
        agregado = m not in base_user
        rec = audited.get(m, {})
        b, w = rec.get("bcn") or {}, rec.get("wikipedia") or {}

        def res(f):
            if b.get(f) is not None:
                return b.get(f), "bcn"
            if w.get(f) is not None:
                return w.get(f), "wiki"
            return None, "base"

        prof = {"nombre_armonizado": m}
        origen = {}

        g, src = res("genero")
        prof["es_mujer"], origen["es_mujer"] = \
            ({"F": 1, "M": 0}[g], src) if g in ("F", "M") else (base.get("es_mujer", 0), "base")

        af, src = res("afiliacion_politica")
        if af is not None:
            # sin churn de formato: si el valor base es semánticamente igual, conservarlo
            if norm_party(base.get("afiliacion_agrupada")) == norm_party(af):
                prof["afiliacion_agrupada"], origen["afiliacion_agrupada"] = base["afiliacion_agrupada"], "base"
            else:
                prof["afiliacion_agrupada"], origen["afiliacion_agrupada"] = af, src
        else:
            prof["afiliacion_agrupada"], origen["afiliacion_agrupada"] = \
                base.get("afiliacion_agrupada", "Desconocida"), "base"

        d, src = res("distrito")
        prof["distrito"] = harmonize_district(base.get("distrito"), d)
        origen["distrito"] = src if prof["distrito"] != base.get("distrito") else "base"

        ab, src = res("es_abogado")
        prof["es_abogado"], origen["es_abogado"] = \
            (int(bool(ab)), src) if ab is not None else (base.get("es_abogado", 0), "base")

        fn, src = res("fecha_nacimiento")
        edad = age_at(fn) if fn else None
        prof["edad_al_asumir"], origen["edad_al_asumir"] = \
            (edad, src) if edad is not None else (base.get("edad_al_asumir"), "base")

        gr, src = res("grado_academico_nivel")
        prof["grado_academico_nivel"], origen["grado_academico_nivel"] = \
            (int(gr), src) if gr is not None else (base.get("grado_academico_nivel", 0), "base")

        ex, src = res("experiencia_previa_institucional")
        prof["experiencia_previa_institucional"], origen["experiencia_previa_institucional"] = \
            (int(bool(ex)), src) if ex is not None else (base.get("experiencia_previa_institucional", 0), "base")

        # capa final: validaciones manuales del usuario (siempre prevalecen)
        for campo in list(prof.keys()):
            if campo == "nombre_armonizado":
                continue
            key = (m, campo)
            if key in manual:
                if prof[campo] != manual[key]:
                    conflicto = origen[campo] in ("bcn", "wiki")
                    report.append([m, campo, prof[campo], manual[key],
                                   "manual_sobre_" + origen[campo] +
                                   ("_CONFLICTO" if conflicto else "")])
                prof[campo], origen[campo] = manual[key], "manual"

        # registrar cambios respecto del archivo previo
        prev = base_user.get(m)
        if agregado:
            report.append([m, "(perfil completo)", None, "agregado", "nuevo"])
        elif prev:
            for campo, v in prof.items():
                if campo != "nombre_armonizado" and prev.get(campo) != v:
                    report.append([m, campo, prev.get(campo), v, origen[campo]])
        out.append(prof)

    json.dump(out, open(PROFILES_OUT, "w", encoding="utf-8"), ensure_ascii=False, indent=4)
    with open(os.path.join(OUT_DIR, "imputation_report.csv"), "w", newline="", encoding="utf-8") as fh:
        wr = csv.writer(fh)
        wr.writerow(["nombre", "campo", "valor_previo", "valor_nuevo", "origen"])
        wr.writerows(report)
    n_manual = sum(1 for r in report if str(r[4]).startswith("manual"))
    print(f"  perfiles imputados: {len(out)} -> {PROFILES_OUT}")
    print(f"  cambios registrados: {len(report)} (de ellos {n_manual} decididos por validación manual)")


if __name__ == "__main__":
    members, raw = load_inputs()
    os.makedirs(OUT_DIR, exist_ok=True)
    if "--test" in sys.argv:
        run_batches(members, raw, test=True)
        print(open(os.path.join(BATCH_DIR, "batch_999.json"), encoding="utf-8").read())
    elif "--tables" in sys.argv:
        build_tables(members)
    elif "--impute" in sys.argv:
        compare_runs(members)
        impute_profiles(members)
    else:
        run_batches(members, raw)
        build_tables(members)
        compare_runs(members)
        impute_profiles(members)
