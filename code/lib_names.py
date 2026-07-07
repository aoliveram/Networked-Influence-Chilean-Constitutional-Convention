"""
lib_names.py — Módulo único de normalización y validación de nombres (cierra P4).

Todo autor que entra al pipeline pasa por normalize_author(), que resuelve contra
los 154 nombres canónicos de convention_members.json. Reglas, en orden:
  1. corrección explícita (NAME_CORRECTIONS),
  2. match exacto tras normalizar tildes/espacios/mayúsculas,
  3. apellido solo, si es único en el roster (p. ej. "Roa" -> "Roa, Giovanna"),
  4. no-persona: referencias a iniciativas populares ("7-2", "Iniciativa Popular
     Indígena 21-2") se etiquetan como tales y quedan FUERA de la red de personas.
Lo que no resuelve queda como 'unmatched' y debe revisarse (nunca descartarse en
silencio): usar el colector de clean_authors o el QA 0a.
"""

import json
import os
import re
import unicodedata

from paths import MEMBERS

# correcciones explícitas: variantes observadas en los datos -> nombre canónico
NAME_CORRECTIONS = {
    "Muñoz, Pedro": "Munoz, Pedro",
    "Sepúlveda, Barbara": "Sepulveda, Barbara",
    "Sepúlveda, Carolina": "Sepulveda, Carolina",
    "Vargas, Margaritfa": "Vargas, Margarita",
    "Chinga": "Chinga, Eric",
    "Dayyana, Gonzalez": "Gonzalez, Dayana",   # invertido + variante ortográfica
    "Lidia, Gonzalez": "Gonzalez, Lidia",      # invertido
}

# referencias a iniciativas populares/indígenas registradas como "autor"
NON_PERSON_RE = re.compile(r"^\d+(-\d+)?$|iniciativa(s)? popular(es)?", re.IGNORECASE)

CANONICAL = json.load(open(MEMBERS, encoding="utf-8"))
assert len(CANONICAL) == 154, f"convention_members.json debe tener 154 nombres, tiene {len(CANONICAL)}"


def _norm(s):
    s = unicodedata.normalize("NFKD", str(s)).encode("ascii", "ignore").decode()
    return re.sub(r"\s+", " ", s).strip().lower()


_BY_NORM = {_norm(c): c for c in CANONICAL}

_surnames = {}
for c in CANONICAL:
    _surnames.setdefault(_norm(c.split(",")[0]), []).append(c)
_BY_UNIQUE_SURNAME = {s: v[0] for s, v in _surnames.items() if len(v) == 1}


def normalize_author(raw):
    """Devuelve (canónico | None, status).

    status: 'ok' (resuelto a canónico), 'non_person' (iniciativa popular),
            'empty', 'unmatched'.
    """
    if raw is None:
        return None, "empty"
    s = re.sub(r"\s+", " ", str(raw)).strip()
    if not s or len(s) <= 1 or "S/I" in s:
        return None, "empty"
    if NON_PERSON_RE.search(s):
        return None, "non_person"
    s = NAME_CORRECTIONS.get(s, s)
    n = _norm(s)
    if n in _BY_NORM:
        return _BY_NORM[n], "ok"
    if "," not in n and n in _BY_UNIQUE_SURNAME:
        return _BY_UNIQUE_SURNAME[n], "ok"
    return None, "unmatched"


def clean_authors(authors_list, collector=None):
    """Lista (posiblemente anidada) de strings crudos -> set ordenado de canónicos.

    Las no-personas y vacíos se descartan; los 'unmatched' se descartan pero se
    acumulan en collector (dict) si se entrega — el QA falla si aparecen nuevos.
    """
    out = set()
    stack = list(authors_list or [])
    while stack:
        a = stack.pop()
        if isinstance(a, list):
            stack.extend(a)
            continue
        canon, status = normalize_author(a)
        if status == "ok":
            out.add(canon)
        elif status == "unmatched" and collector is not None:
            collector[str(a).strip()] = collector.get(str(a).strip(), 0) + 1
    return sorted(out)
