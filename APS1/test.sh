make 
make eval 

# Définition du répertoire principal
main_directory="./Samples"

# Fonction récursive pour parcourir les sous-répertoires
traverse_directories() {
    local dir_path="$1"
    local indent="$2"
    
    # Boucle à travers chaque sous-répertoire dans le répertoire spécifié
    for sub_dir in "$dir_path"/*; do
        if [ -d "$sub_dir" ]; then
            echo "${indent}Traitement du sous-répertoire $(basename "$sub_dir") ..."
            for file in "$sub_dir"/*; do
                if [ -f "$file" ]; then
                    # Exécuter la commande spécifiée
                    t_res=$(./prologTerm "$file" | swipl -s checker.pl -g  main_stdin 2>&1)
                    if [[ $t_res = *"void"* ]]; then 
                        echo "${indent}$(basename "$file")  : Bien typé"
                    else 
                        echo "${indent}$(basename "$file")  : Mal typé"
                    fi
                    res_eval=$(./eval "$file")
                    if [[ $res_eval = *"eval"* ]]; then 
                        echo "${indent}$(basename "$file") : Eval Correct"
                    else 
                        echo "${indent}$(basename "$file") : Eval Incorrect"
                    fi
                fi
            done
            traverse_directories "$sub_dir" "$indent  "
        fi
    done
}

# Vérifier si le répertoire principal existe
if [ -d "$main_directory" ]; then
    echo "##########################################################"
    echo "Traitement du répertoire principal : $main_directory"
    
    # Appeler la fonction pour parcourir les sous-répertoires
    traverse_directories "$main_directory" "  "
else
    echo "Le répertoire principal $main_directory n'existe pas."
fi