# Rapport de correction — Concordancier spaCy / lexique_fr

## Problème observé
Le concordancier échouait avec des erreurs regex de type `Concordancier spaCy : erreur regex sur motif [...]`.
Conséquence : aucun (ou très peu de) mots surlignés dans le HTML.

## Cause technique
Les motifs étaient regroupés dans de très grands regex alternatifs. Selon la taille/complexité des termes significatifs (accents, underscores, numéros, etc.), certains motifs devenaient invalides pour PCRE/Unicode.

## Correctifs appliqués
1. **Préparation des motifs renforcée et séparée par moteur**
   - `concordancier_spacy.R` (spaCy)
   - `concordancier_lexique.R` (lexique_fr)

2. **Validation des motifs de lot + fallback unitaire**
   - Construction d'un motif par lot.
   - Si le motif de lot est invalide, fallback vers des motifs unitaires valides terme par terme.

3. **Réduction de la taille des lots**
   - `taille_lot` par défaut réduit à `80`.
   - Appels de génération HTML ajustés à `taille_lot = 80` pour limiter les motifs trop longs.

4. **Nettoyage de l'entrée des termes**
   - `as.character`, `trimws`, suppression des vides/NA, déduplication.

## Effet attendu
- Fin des erreurs regex de compilation sur les motifs volumineux.
- Retour du surlignage des termes significatifs en mode spaCy et lexique_fr.
- Meilleure robustesse sur des corpus contenant des tokens complexes.
