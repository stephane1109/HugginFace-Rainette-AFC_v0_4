### Analyse morphosyntaxique avec spaCy et Lexique (fr)

- Documentation principale spaCy : <https://spacy.io/usage>
- Linguistic Features (POS, morphology) : <a href="https://spacy.io/usage/linguistic-feature/" target="_blank" rel="noopener noreferrer">Lexique POS spaCy</a>
- Documentation OpenLexicon : <a href="https://openlexicon.fr/" target="_blank" rel="noopener noreferrer">OpenLexicon</a>

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

Quand la **source de lemmatisation** est réglée sur **Lexique (fr)**, le filtrage morphosyntaxique utilise les catégories morphologiques du lexique (`c_morpho`), et non les POS de spaCy.

- **NOM** : maison, analyse
- **NOM_SUP** : alentours
- **VER** : mange, observe
- **VER_SUP** : croie
- **AUX** : a, est
- **ADJ** : grand, important
- **ADJ_SUP** : bis
- **ADJ_DEM** : cet, ces
- **ADJ_IND** : aucun
- **ADJ_INT** : quel
- **ADJ_NUM** : deux, cent
- **ADJ_POS** : mon, leurs
- **ADV** : a_priori, a_capella
- **ADV_SUP** : afin
- **PRE** : dans, avec
- **CON** : et, mais
- **ART_DEF** : le, la, au
- **ART_IND** : un, des
- **PRO_DEM** : ceci, celui_ci
- **PRO_IND** : autre
- **PRO_PER** : je, nous
- **PRO_POS** : mien
- **PRO_REL** : auquel, qui
- **ONO** : ah, badabam

Flux technique (mode Lexique):
1. tokenisation locale (quanteda),
2. filtrage des tokens par présence dans lexique_fr avec les catégories `c_morpho` sélectionnées,
3. lemmatisation (si activée) directement via lexique_fr (forme -> lemme).

> Le filtrage morphosyntaxique lexique_fr est donc indépendant de spaCy.

### Activation de filtrage morphosyntaxique

- de choisir la langue spaCy (`fr`, `en`, `es`, `it`, `de`) quand la source est **spaCy**,
- de sélectionner les POS à conserver parmi la liste Universal POS quand la source est **spaCy**,
- de sélectionner directement les catégories `c_morpho` à conserver quand la source est **Lexique (fr)**,
- de combiner ce filtrage avec la lemmatisation selon les besoins analytiques.
