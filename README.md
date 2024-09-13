# Lambda Calculus Interpreter

## Descriere

Acest proiect implementează un interpretator pentru **Lambda Calculus** în Haskell. Lambda Calculus este un formalism matematic folosit pentru definirea funcțiilor și calculului. Interpretatorul permite evaluarea expresiilor lambda, gestionarea macrocomenzilor și oferă un context implicit cu valori utile predefinite, precum combinatorii, booleeni și operații pe numere naturale.

**Scopul principal** al acestui proiect este de a oferi o platformă pentru explorarea teoretică a expresiilor lambda și a conceptelor legate de calculul funcțional. Proiectul include funcționalități avansate, precum evaluarea pas cu pas și reducerea normală sau aplicativă.

Cerința completă poate fi găsită la https://ocw.cs.pub.ro/ppcarte/doku.php?id=pp:2024:tema3

## Funcționalități

- **Evaluarea expresiilor lambda**: Suport pentru variabile, abstracții și aplicații lambda.
- **Context definit de utilizator**: Definiți macrocomenzi care pot fi folosite în alte expresii.
- **Suport pentru reducere**: Evaluare folosind reducerea normală sau aplicativă.
- **Gestionarea contextului**: Puteți afișa și reseta contextul curent.
- **Valori implicite predefinite**: Include combinatori clasici, funcții booleene și operații pe numere naturale.

## Implementare

Proiectul este organizat în mai multe fișiere Haskell, fiecare având o responsabilitate clară:

- **Main.hs**: Acesta este punctul de intrare în interpretator și conține funcția `main` care gestionează bucla de interpretare. De asemenea, oferă suport pentru gestionarea input-urilor de la utilizator și pentru comenzi speciale (ieșire, resetare context, afișare context).

- **Lambda.hs**: Definește structura de date pentru expresiile lambda și include funcțiile esențiale pentru manipularea acestora. Aceste funcții includ:
  - `vars`: extrage toate variabilele dintr-o expresie lambda.
  - `freeVars`: calculează variabilele libere ale unei expresii.
  - `reduce`: funcția de reducere a unei aplicații lambda.
  - `normalStep` și `applicativeStep`: definesc pașii de reducere normală și aplicativă.

- **Parser.hs**: Implementarea unui parser pentru Lambda Calculus. Permite interpretarea expresiilor lambda din format text și suportă variabile, macrocomenzi și abstracții.

- **Binding.hs**: Gestionează contextul de macrocomenzi și oferă funcționalități pentru substituirea acestora în expresii lambda. Conține și funcțiile `normalCtx` și `applicativeCtx`, care evaluează expresii folosind reducerea normală sau aplicativă.

- **Default.hs**: Definește un context implicit cu macrocomenzi predefinite. Acesta include combinatori de bază (e.g., `I`, `K`, `Y`), funcții booleene (e.g., `TRUE`, `FALSE`, `AND`, `OR`) și operații pe numere naturale (e.g., `N0`, `N1`, `SUCC`, `ADD`, `MULT`).
