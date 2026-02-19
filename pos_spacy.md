### Analyse morphosyntaxique avec spaCy

- Documentation principale : <https://spacy.io/usage>
- Linguistic Features (POS, morphology) : <https://spacy.io/usage/linguistic-features>

### Traduction FR des POS (spaCy / Universal POS)

- **ADJ** : adjectif
- **ADP** : adposition (préposition)
- **ADV** : adverbe
- **AUX** : auxiliaire
- **CCONJ** : conjonction de coordination
- **DET** : déterminant
- **INTJ** : interjection
- **NOUN** : nom
- **NUM** : numéral
- **PART** : particule
- **PRON** : pronom
- **PROPN** : nom propre
- **PUNCT** : ponctuation
- **SCONJ** : conjonction de subordination
- **SYM** : symbole
- **VERB** : verbe
- **X** : autre / catégorie inconnue

### Filtrage morphosyntaxique spécifique OpenLexicon

Quand la **source de lemmatisation** est réglée sur **Lexique (fr)**, le filtrage morphosyntaxique utilise maintenant les catégories `Lexique4__Cgram` d’`OpenLexicon.csv` (et non les POS Universal spaCy).

- Catégories usuelles : `NOM`, `VER`, `AUX`, `ADJ`, `ADV`, `PRE`, `CON`.
- Catégories fines disponibles : `PRO:*`, `ADJ:*`, `ART:*`, `ONO`.
- Valeur de départ recommandée : `NOM`, `VER`, `ADJ`.

Flux technique :
1. spaCy tokenise/annote le texte,
2. les tokens sont filtrés par présence dans OpenLexicon avec les `Cgram` sélectionnés,
3. la lemmatisation (si activée) est résolue via OpenLexicon avec correspondance POS spaCy → Cgram Lexique.

### Paramétrage côté interface (Shiny)

Dans l’interface, la section **Paramétrages SpaCy** permet :

- d’activer le **filtrage morphosyntaxique**,
- de sélectionner les POS à conserver parmi la liste Universal POS quand la source est **spaCy**,
- de sélectionner les catégories **OpenLexicon (Lexique4__Cgram)** quand la source est **Lexique (fr)**,
- de combiner ce filtrage avec la lemmatisation selon les besoins analytiques.

### Conseils pratiques

- Pour une analyse thématique : commencer par `NOUN,VERB,ADJ` (spaCy) ou `NOM,VER,ADJ` (OpenLexicon).
- Pour préserver les noms d’organisations/personnes : ajouter `PROPN` (spaCy) ou conserver `NOM` (OpenLexicon).
- Pour éviter le bruit grammatical : exclure en général `DET`, `PRON`, `CCONJ`, `SCONJ`, `PART` (spaCy), ou éviter `ART:*`, `PRO:*`, `CON` (OpenLexicon) si l’objectif est thématique.
