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

### Filtrage morphosyntaxique spécifique lexique_fr

Quand la **source de lemmatisation** est réglée sur **Lexique (fr)**, le filtrage morphosyntaxique utilise les catégories morphologiques du lexique (`c_morpho`), et non les POS Universal spaCy.

- Catégories usuelles : `NOM`, `VER`, `AUX`, `ADJ`, `ADV`, `PRE`, `CON`.
- Catégories fines disponibles : `PRO:*`, `ADJ:*`, `ART:*`, `ONO`.
- Valeur de départ recommandée : `NOM`, `VER`, `ADJ`.

| Forme | Exemple |
|---|---|
| `NOM` | `maison`, `analyse` |
| `VER` | `manger`, `observe` |
| `AUX` | `être`, `avoir` |
| `ADJ` | `grand`, `important` |
| `ADV` | `rapidement`, `souvent` |
| `PRE` | `dans`, `avec` |
| `CON` | `et`, `mais` |
| `PRO:*` | `je`, `nous`, `celui-ci` |
| `ADJ:*` | `possessif` (`mon`), `démonstratif` (`ce`) |
| `ART:*` | `le`, `un`, `des` |
| `ONO` | `bam`, `ouf` |

Flux technique (mode Lexique):
1. tokenisation locale (quanteda),
2. filtrage des tokens par présence dans lexique_fr avec les catégories `c_morpho` sélectionnées,
3. lemmatisation (si activée) directement via lexique_fr (forme -> lemme).

> Le filtrage morphosyntaxique lexique_fr est donc indépendant de spaCy.

### Paramétrage côté interface (Shiny)

Dans l’interface, la section **Paramétrages SpaCy** permet :

- d’activer le **filtrage morphosyntaxique**,
- de choisir la langue spaCy (`fr`, `en`, `es`) quand la source est **spaCy**,
- de sélectionner les POS à conserver parmi la liste Universal POS quand la source est **spaCy**,
- de sélectionner directement les catégories `c_morpho` à conserver quand la source est **Lexique (fr)**,
- de combiner ce filtrage avec la lemmatisation selon les besoins analytiques.

### Conseils pratiques

- Pour une analyse thématique : commencer par `NOUN,VERB,ADJ` (spaCy) ou `NOM,VER,ADJ` (lexique_fr).
- Pour préserver les noms d’organisations/personnes : ajouter `PROPN` (spaCy) ou conserver `NOM` (lexique_fr).
- Pour éviter le bruit grammatical : exclure en général `DET`, `PRON`, `CCONJ`, `SCONJ`, `PART` (spaCy), ou éviter `ART:*`, `PRO:*`, `CON` (lexique_fr) si l’objectif est thématique.
