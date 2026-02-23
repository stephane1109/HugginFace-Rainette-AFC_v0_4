[//]: # (Rôle du fichier: rapport_dedoublement_pipelines_spacy_lexique_2026-02-23.md documente une partie de l'application Rainette.)
[//]: # (Ce document sert de référence fonctionnelle/technique pour l'équipe.)
[//]: # (Il décrit le comportement attendu afin de sécuriser maintenance et diagnostics.)
# Rapport — Dédoublement des pipelines d'analyse (spaCy vs lexique_fr)

## Demande
Séparer explicitement l'analyse selon le choix utilisateur :
- pipeline complet **spaCy**
- pipeline complet **lexique_fr**

## Ce qui a été fait

### 1) Fichiers dédiés par moteur
- Ajout de `R/pipeline_spacy_analysis.R` : fonction `executer_pipeline_spacy()`.
- Ajout de `R/pipeline_lexique_analysis.R` : fonction `executer_pipeline_lexique()`.

### 2) Dispatcher unique dans le serveur
Dans `R/server_events_lancer.R`, le prétraitement/DFM n'est plus un bloc mixte unique.
Le code appelle désormais :
- `executer_pipeline_lexique(...)` si `source_dictionnaire == "lexique_fr"`
- `executer_pipeline_spacy(...)` sinon

Chaque pipeline renvoie son résultat propre (`tok`, `dfm_obj`, `langue_reference`, `source_dictionnaire`).

### 3) Chargement explicite des modules
`app.R` source maintenant les deux nouveaux fichiers pipeline :
- `R/pipeline_spacy_analysis.R`
- `R/pipeline_lexique_analysis.R`

## Bénéfices
- Séparation claire des responsabilités (plus de logique imbriquée difficile à maintenir).
- Réduction du risque de régression croisée entre spaCy et lexique_fr.
- Traçabilité plus simple lors du debug (logs et comportements distincts par pipeline).

## Limites de validation locale
Le runtime R n'est pas disponible dans cet environnement (`Rscript` absent), donc validation dynamique non exécutable ici.
La validation effectuée est structurelle (diffs + inspection des zones modifiées).
