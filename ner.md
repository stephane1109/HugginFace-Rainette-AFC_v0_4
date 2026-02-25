# NER (spaCy + règles JSON)

## Fonctionnement (résumé)
1. spaCy détecte des entités (PER, ORG, LOC, etc.).
2. Un filtrage supprime des faux positifs (ponctuation seule, cas bruités, etc.).
3. Si un JSON est fourni, ses règles sont appliquées : exclusion et ajouts.

## Activer un dictionnaire JSON
### Option 1 (recommandée) : upload via l'interface
1. Utiliser le champ **Dictionnaire NER JSON (optionnel)** dans la section *Paramètres SpaCy/NER*.
2. Cocher **Activer NER (spaCy)** puis lancer l'analyse.

Le champ d'upload est visible en permanence ; le fichier n'est utilisé que lors d'une analyse NER.

### Option 2 : variable d'environnement (fallback)
Définir la variable avant de lancer l'app :

```bash
export RAINETTE_NER_JSON=/chemin/vers/mon_ner.json
```

Si un fichier est uploadé dans l'UI, il est prioritaire sur la variable d'environnement.

## Structure du JSON
Exemple :

```json
{
  "exclude_texts": ["ça", "«", "»"],
  "exclude_labels": ["MISC"],
  "include": [
    {"text": "OpenAI", "label": "ORG"},
    {"text": "ChatGPT", "label": "PRODUCT"}
  ]
}
```

## Signification des champs JSON
- `exclude_texts` : liste de textes d'entité à **rejeter** (comparaison normalisée, insensible à la casse).
- `exclude_labels` : liste de labels d'entité à **rejeter** (ex: `MISC`).
- `include` : liste d'entités à **forcer**.
  - `text` : texte recherché dans le document.
  - `label` : label assigné à l'entité ajoutée.

## Expressions utilisées (important)
Pour `include`, le script utilise une regex Python de la forme :

- `\b<text>\b` avec `re.IGNORECASE`.

Cela veut dire :
- recherche **insensible à la casse** ;
- correspondance sur des **bornes de mot** (`\b`) ;
- évite de matcher au milieu d'un mot.

Exemple : `"text": "Paris"` matche `Paris` mais pas `parisien`.

## Bonnes pratiques
- Commencer petit (quelques exclusions très fréquentes).
- Ajouter `exclude_labels` seulement si nécessaire (peut être trop agressif).
- Vérifier les logs NER pour confirmer que le JSON est chargé.
