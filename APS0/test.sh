make 
make eval 
directories=("defs" "exprs" "prim")
main_directory="./Samples"

# Boucle à travers chaque répertoire
for dir in "${directories[@]}"
do
    # Vérifier si le répertoire existe
    full_dir="$main_directory/$dir"
    if [ -d "$full_dir" ]; then
        echo "##########################################################"
        echo "Traitement du répertoire $dir ..."
        
        # Boucle à travers chaque fichier dans le répertoire
        for file in "$full_dir"/*
        do
            # Vérifier si le fichier est un fichier ordinaire
            if [ -f "$file" ]; then
                
                # Exécuter la commande spécifiée
                t_res=$(./prologTerm "$file" | swipl -s checker.pl -g  main_stdin 2>&1)
                if [[ $t_res = *"void"* ]]; then 
                    echo "$(basename "$file")  : Typed checked Correct"
                else 
                    echo "$(basename "$file")  : Typed checked Incorrect"
                fi
                res_eval=$(./eval "$file")
                if [[ $res_eval = *"eval"* ]]; then 
                    echo "$(basename "$file") : Eval Correct"
                else 
                    echo "$(basename "$file") : Eval Incorrect"
                fi
            fi
        done
    else
        echo "Le répertoire $dir n'existe pas."
    fi
done