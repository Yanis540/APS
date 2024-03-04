#!/bin/bash

make
make eval

# Définition du répertoire principal
main_directory="./Samples"

# Nom du répertoire à exclure
exclude_directory="archive"

# Nom du fichier pour enregistrer les erreurs
error_file="errors.txt"
# Effacer le contenu du fichier d'erreurs
> "$error_file"

# Fonction récursive pour parcourir les sous-répertoires et détecter les erreurs
traverse_directories() {
    local dir_path="$1"
    local indent="$2"
    
    # Boucle à travers chaque sous-répertoire dans le répertoire spécifié
    for sub_dir in "$dir_path"/*; do
        if [ -d "$sub_dir" ] && [ "$(basename "$sub_dir")" != "$exclude_directory" ]; then
            echo -e "${indent}Traitement du sous-répertoire $(basename "$sub_dir") ..."
            for file in "$sub_dir"/*; do
                if [ -f "$file" ]; then
                    # Exécuter la commande spécifiée et capturer les erreurs
                    t_res=$(./prologTerm "$file" | swipl -s checker.pl -g  main_stdin 2>&1)
                    if [[ $t_res = *"void"* ]]; then 
                        echo -e "${indent}\t$(basename "$file")  : Bien typé"
                    else 
                        echo -e "${indent}\t$(basename "$file")  : Mal typé"
                    fi
                    res_eval=$(./eval "$file" 2>&1)
                    if [[ $res_eval = "Fatal error: exception Failure"* ]]; then 
                        echo -e "${indent}\t$(basename "$file") : Eval Incorrect"
                        echo "$(basename "$file") : $res_eval" >> "$error_file"
                    else 
                        echo -e "${indent}\t$(basename "$file") : Eval Correct"
                    fi
                fi
            done
            traverse_directories "$sub_dir" "${indent}\t"
        fi
    done
}

# Vérifier si le répertoire principal existe
if [ -d "$main_directory" ]; then
    echo "##########################################################"
    echo "Traitement du répertoire principal : $main_directory"
    
    # Appeler la fonction pour parcourir les sous-répertoires
    traverse_directories "$main_directory" "\t"
else
    echo "Le répertoire principal $main_directory n'existe pas."
fi
