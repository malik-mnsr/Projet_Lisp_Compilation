# Compilateur & Machine Virtuelle

## Auteurs
- **BELLIL Mohamed Nadir** - [mohamed.bellil@etu.umontpellier.fr](mailto:mohamed.bellil@etu.umontpellier.fr)
- **MANSOUR Malik** - [malik.mansour@etu.umontpellier.fr](mailto:malik.mansour@etu.umontpellier.fr)

---

## Introduction

Ce projet implémente :
- **Un compilateur** : Convertit une expression Lisp en un fichier assembleur (`.asm`).
- **Une machine virtuelle (VM)** : Interprète les fichiers `.asm` générés par le compilateur pour exécuter les instructions et produire un résultat final.

---

## Utilisation Générale

### Compilateur
Pour utiliser le compilateur, procédez comme suit :
1. Chargez le fichier du compilateur dans Lisp :
   ```lisp
   (load "compilateur.lisp")
2. Exécutez la fonction de compilation avec le nom du fichier d'entrée et le nom du fichier de
sortie :
   ```lisp
   (compilation "NomFichierEntree.lisp" "NomFichierSortie.asm") 
   Exemple : (compilation "fibo.lisp" "fibo.asm")
### Machine Virtuelle

Voici la procédure pour utiliser la Machine Virtuelle :

1. **Chargez le fichier de la machine virtuelle** :
   ```lisp
   (load "machine.lisp")
2. **Créez une nouvelle instance de la machine virtuelle** :
   ```lisp
   (vm-creation 'mv)
3. **Chargez le fichier asm dans la machine virtuelle** :
   ```lisp
   (vm-load 'mv "CheminFichier.asm") 
   Exemple : (vm-load 'mv "ASMFILES/fibo.asm")
4. **Exécutez le code à l'intérieur de la machine virtuelle** :
    ```lisp
   (vm-run-code 'mv)
### Fichiers de programme automatiques
Dans le répertoire `Auto`, il existe deux fichiers, `testFACT` et `testFIBO`. Vous pouvez charger l'un de ces
deux fichiers. Ils chargeront les fichiers du compilateur et de la VM, créeront une nouvelle instance de
la VM et chargeront et exécuteront le fichier de test.