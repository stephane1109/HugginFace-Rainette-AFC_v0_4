# NER (spaCy + règles JSON)

## Fonctionnement
1. spaCy détecte des entités (PER, ORG, LOC, etc...).
2. Un mini-filtrage supprime des faux positifs (ponctuation seule, cas bruités, etc.).
3. Si un JSON est fourni, ses règles sont appliquées : exclusions et ajouts.

## Importer un dictionnaire JSON dans l'UI
1. Coche **Activer NER (spaCy)** dans la barre latérale.
2. Dans le champ **Importer un dictionnaire NER (.json)**, sélectionne ton fichier local.
3. Lance (ou relance) l'analyse avec **Lancer l'analyse**.

Notes :
- Le fichier doit être au **format `.json`**.
- L'import via l'UI est **optionnel** : si aucun fichier n'est importé, l'application tente d'utiliser la variable d'environnement `RAINETTE_NER_JSON` si elle est définie.

## Alternative sans UI (variable d'environnement)
Tu peux aussi définir la variable d'environnement avant de lancer l'app :

```bash
export RAINETTE_NER_JSON=/chemin/vers/mon_ner.json
```

Le pipeline R passera automatiquement ce fichier à `ner.py`.

## Format attendu du fichier JSON
Exemple valide :

```json
{
  "exclude_texts": ["ça", "«", "»"],
  "exclude_labels": ["MISC"],
  "include": [
    {"text": "OpenAI", "label": "ORG"},
    {"text": "ChatGPT", "label": "PRODUCT"},
    {"text": "regarder", "label": "ACTION"},
    {"text": "commencer", "label": "ACTION"}
  ]
}
```


## Peut-on créer ses propres labels ?
Oui, via `include`.

- Les entités détectées *nativement* par spaCy gardent les labels du modèle (`PER`, `ORG`, `LOC`, etc.).
- Les entités ajoutées via `include` peuvent utiliser **n'importe quel label** (ex: `ACTION`, `OUTIL`, `THEME`).
- Ces labels personnalisés apparaissent ensuite dans la sortie NER (`ent_label`).

Exemple: `{"text": "commencer", "label": "ACTION"}` forcera la présence de `commencer` avec le label `ACTION` si le mot est trouvé dans le texte.

⚠️ Format strict des entrées `include`:
- chaque entrée doit être un objet avec `text` (obligatoire) et `label` (optionnel, défaut `MISC`) ;
- les autres clés ne sont pas acceptées ;
- si `text` est vide, le JSON est rejeté avec message d'erreur explicite.

## Signification des champs JSON
- `exclude_texts` : liste de textes d'entité à **rejeter** (insensible à la casse).
- `exclude_labels` : liste de labels d'entité à **rejeter** (ex. `MISC`).
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
- Commencer petit (quelques exclusions fréquentes).
- Ajouter `exclude_labels` seulement si nécessaire (peut être trop agressif).
- Vérifier les logs NER et le statut NER pour confirmer que le JSON est chargé.
