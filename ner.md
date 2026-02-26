# NER (spaCy + règles JSON)

## Fonctionnement
1. spaCy détecte des entités (PER, ORG, LOC, etc...).
2. Un mini-filtrage supprime des faux positifs (ponctuation seule, cas bruités, etc...).
3. Si un JSON est fourni, ses règles sont appliquées : exclusions et ajouts.

## Importer un dictionnaire JSON dans l'UI
1. Coche **Activer NER (spaCy)** dans la barre latérale.
2. Dans le champ **Importer un dictionnaire NER (.json)**, sélectionne ton fichier local.
3. Lance (ou relance) l'analyse avec **Lancer l'analyse**.

## Format attendu du fichier JSON
- Le fichier doit être au **format `.json`**.
Exemple totalement farfellu montrabt que vous pouvais exclure, inclure des mots definir le label selon les labels deja existant, soit créer un nouveau label :

```json
{
  "exclude_texts": ["ça", "«", "»"],
  "exclude_labels": ["MISC"],
  "include": [
    {"text": "OpenAI", "label": "ORG"},
    {"text": "ChatGPT", "label": "PRODUCT"},
    {"text": "regarder", "label": "VERBE"},
    {"text": "commencer", "label": "VERBE"}
  ]
}
```


## Peut-on créer ses propres labels ?
Oui

- Les entités détectées *nativement* par spaCy gardent les labels du modèle (`PER`, `ORG`, `LOC`, etc.).
- Les entités ajoutées via `include` peuvent utiliser **n'importe quel label** (ex: `ACTION`, `OUTIL`, `THEME`).
- Ces labels personnalisés apparaissent ensuite dans la sortie NER (`ent_label`).

Exemple: `{"text": "commencer", "label": "ACTION"}` forcera la présence de `commencer` avec le label `ACTION` si le mot est trouvé dans le texte.

## Labels spaCy déjà existants
Les labels disponibles dépendent du **modèle spaCy chargé**.

### Labels du modèle FR utilisé dans ce projet (`fr_core_news_md`)
- `PER` : personne
- `ORG` : organisation
- `LOC` : lieu
- `MISC` : catégorie diverse (autres entités)

### Labels NER officiels spaCy (OntoNotes)
- `PERSON`: People, including fictional.
- `NORP`: Nationalities or religious or political groups.
- `FAC`: Buildings, airports, highways, bridges, etc.
- `ORG`: Companies, agencies, institutions, etc.
- `GPE`: Countries, cities, states.
- `LOC`: Non-GPE locations, mountain ranges, bodies of water.
- `PRODUCT`: Objects, vehicles, foods, etc. (Not services.)
- `EVENT`: Named hurricanes, battles, wars, sports events, etc.
- `WORK_OF_ART`: Titles of books, songs, etc.
- `LAW`: Named documents made into laws.
- `LANGUAGE`: Any named language.
- `DATE`: Absolute or relative dates or periods.
- `TIME`: Times smaller than a day.
- `PERCENT`: Percentage, including ”%“.
- `MONEY`: Monetary values, including unit.
- `QUANTITY`: Measurements, as of weight or distance.
- `ORDINAL`: “first”, “second”, etc.
- `CARDINAL`: Numerals that do not fall under another type.

💡 Astuce : dans ce projet, les labels issus du JSON sont normalisés en majuscules, donc écris de préférence les labels en MAJUSCULES (`ORG`, `PER`, `PERSON`, etc.).

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
