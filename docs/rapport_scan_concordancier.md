[//]: # (Rôle du fichier: rapport_scan_concordancier.md documente une partie de l'application Rainette.)
[//]: # (Ce document sert de référence fonctionnelle/technique pour l'équipe.)
[//]: # (Il décrit le comportement attendu afin de sécuriser maintenance et diagnostics.)
# Rapport de scan — concordancier (ciblé sur affichage + surlignage)

## Périmètre scanné
- `concordancier.R`
- `R/server_events_lancer.R` (construction de `res_stats_df` transmis au concordancier)
- `app.R` (colonnes attendues dans les sorties stats)

## Constats
1. Le concordancier filtrait les termes sur une colonne dynamique (`p` ou `p_value`).
2. La demande métier impose explicitement un filtrage sur `p_value`.
3. `res_stats_df` ne garantissait pas toujours l’existence explicite de `p_value` à la source du pipeline.

## Corrections appliquées
1. **Pipeline stats** : ajout explicite de `p_value = p` dans `R/server_events_lancer.R` au moment de construire `res_stats_df`.
2. **Concordancier** : filtrage strict des termes sur `p_value <= max_p`.
3. **Sécurité d’exécution** : si `p_value` est absente malgré tout, message d’erreur clair dans le HTML et log applicatif.
4. **Comportement surlignage** : maintien du flux “concordancier avec surlignage” (pas d’affichage alternatif sans surlignage).

## Résultat attendu
- Le concordancier sélectionne les termes via `p_value` uniquement.
- Les segments affichés restent ceux contenant des termes significatifs, avec surlignage Unicode.
- En cas de structure stats invalide, l’erreur est explicite (au lieu d’un comportement silencieux).
