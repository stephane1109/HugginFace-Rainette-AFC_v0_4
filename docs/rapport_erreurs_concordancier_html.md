[//]: # (Rôle du fichier: rapport_erreurs_concordancier_html.md documente une partie de l'application Rainette.)
[//]: # (Ce document sert de référence fonctionnelle/technique pour l'équipe.)
[//]: # (Il décrit le comportement attendu afin de sécuriser maintenance et diagnostics.)
# Rapport d'erreurs détaillé — concordancier HTML

## Méthode de scan
- Inspection statique de `concordancier.R` (lecture complète).
- Vérification des points d'appel côté serveur (`R/server_events_lancer.R`).
- Tentative de validation syntaxique automatisée impossible dans cet environnement (`Rscript` absent).

## Problèmes détectés

### 1) **Erreur bloquante : fonction non définie `expandir_variantes_termes`**
- **Localisation** : `concordancier.R`, appel direct dans `generer_concordancier_html()`.
- **Preuve** : `termes_a_surligner <- expandir_variantes_termes(termes_a_surligner)`.
- **Impact** : crash immédiat de la génération HTML dès qu'une classe est traitée ; l'export du concordancier échoue.
- **Niveau** : Critique (bloquant).

### 2) **Erreur bloquante : variable non définie `mode_degrade`**
- **Localisation** : `concordancier.R`, condition `if (mode_degrade) { ... }`.
- **Preuve** : aucune définition locale, aucun argument de fonction, aucune assignation globale dans le dépôt.
- **Impact** : même si l'étape précédente passait, l'exécution lève `object 'mode_degrade' not found`.
- **Niveau** : Critique (bloquant).

### 3) **Risque de HTML invalide / injection dans les segments**
- **Localisation** : `concordancier.R`, écriture brute `writeLines(paste0("<p>", seg, "</p>"), con)`.
- **Problème** : les segments ne sont jamais échappés en HTML avant insertion. Si un segment contient `<`, `>`, `&` ou du pseudo-HTML, le rendu peut casser.
- **Impact** : affichage cassé, structure DOM altérée, potentiel XSS si source texte non fiabilisée.
- **Niveau** : Majeur.

### 4) **Masquage silencieux d'erreurs regex (diagnostic difficile)**
- **Localisation** : `concordancier.R`, `tryCatch(..., error = function(e) s_nfd)` dans `surligner_vecteur_html_unicode()`.
- **Problème** : une erreur regex n'est ni remontée ni loggée ; le traitement continue silencieusement.
- **Impact** : concordancier généré sans surlignage attendu, sans message clair pour l'utilisateur.
- **Niveau** : Moyen.

### 5) **Paramètre inutilisé : `explor_assets`**
- **Localisation** : signature de `generer_concordancier_html(..., explor_assets = NULL, ...)`.
- **Problème** : argument transmis côté serveur mais jamais exploité dans la fonction.
- **Impact** : dette technique, confusion lors du debug (on croit enrichir le HTML alors que non).
- **Niveau** : Mineur.

## Cause racine probable de la panne "le concordancier html ne fonctionne plus"
Les deux erreurs critiques (fonction et variable non définies) se trouvent dans le flux principal de `generer_concordancier_html()` et suffisent à expliquer une régression immédiate du concordancier HTML.

## Priorisation de correction
1. Définir ou retirer `expandir_variantes_termes` (corriger l'appel).
2. Définir explicitement `mode_degrade` (ou supprimer la branche).
3. Échapper les segments avant écriture HTML puis réinjecter le surlignage de façon sûre.
4. Logger les erreurs regex au lieu de les ignorer silencieusement.
5. Supprimer `explor_assets` de la signature si non utilisé.
