#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<stdbool.h>
#include"automata.h"
#include<assert.h>

bool automaton_accepts(const automaton *automaton, unsigned const char *word){
    int i = 0;
    int etat = 0;
    while(word[i] != '\0'){
        //printf("hey %c, %d\n", word[i], etat);
        bool found = false;
        for (int j =  automaton->first_transition[word[i]]; j < automaton->first_transition[word[i]+1]; j ++ ){
            if(automaton->transitions[j].src_state == etat){
                etat = automaton->transitions[j].dest_state;
                found = true;
                //printf("%d\n", etat);
                break;
            }
        }
        if (!found){
            return false;
        }
        i = i + 1;
    }
    return automaton->is_final[etat];
}


void copytab(transition* tab1, transition* tab2, int taille){
    for(int j = 0; j < taille; j++){
        tab1[j] = tab2[j];
    }
}


bool is_in(char* str, int i){
    int len = strlen(str)-1;
    for(int j = 0; j < len; j++){
        int c = str[j];
        if(i == c){
            return true;
        }
    }
    return false;
}

automaton creer_atomotate(char* nom_de_fichier){
    FILE* fichier= fopen(nom_de_fichier, "r");
    int size = 8;
    automaton res;
    int* is_final = (int*)malloc(size*sizeof(int));
    for(int j= 0; j < 7; j++){
        fgetc(fichier);
    }
    int ind = 0;
    while(fscanf(fichier, "%d", &is_final[ind])){
        printf("%d\n", is_final[ind]);
        ind = ind + 1;
        if (ind == size){
            int* new = (int*)realloc(is_final, 2*size*sizeof(int));
            is_final = new;
        }
    }
    int nb_winner = ind;
    int* new = realloc(is_final, ind*sizeof(int));
    int taille_vic = ind;
    char acc = fgetc(fichier);
    acc = fgetc(fichier);
    int max_etat = 0;
    int taille = 10000;
    transition* transo = (transition*)malloc(taille*sizeof(transition));
    int nb_transi = 0;
    for(int i = 0; i <257; i++) {
        res.first_transition[i] = 0;
    }
    int deb=-8;
        int fin;
        unsigned char c;
    while(feof(fichier)==0){        
        int acc = fscanf(fichier, "%d ->[%c] %d;\n", &deb, &c, &fin);
        char nul;
        if (acc == 3){
            nb_transi +=1;
            if (nb_transi == taille){
                taille = 2*taille;
                transo  = (transition*)realloc(transo, 2*size*sizeof(transition));
                
            }
            if (deb > max_etat){
                max_etat = deb;
            }
            if(fin > max_etat){
                max_etat = fin;
            }
            transition t1;
            t1.src_state = deb;
            t1.dest_state = fin;
            int c_int = c;
            for(int i= c_int+1; i < 257; i++){
                res.first_transition[i] = res.first_transition[i] + 1;
            }
            for(int j = res.first_transition[256]-1; j >= res.first_transition[c_int+1]; j--){
                transo[j] = transo[j-1];
            }
            transo[res.first_transition[c_int+1]-1] = t1;
        }
        else{
            char* lettres = (char*)malloc(256*sizeof(char));
            int acc = fscanf(fichier, "![%s] %d;\n",lettres, &fin);
            if(acc == 2){
                if (deb > max_etat){
                max_etat = deb;
                }
                if(fin > max_etat){
                    max_etat = fin;
                }
                nb_transi +=256;
                if (nb_transi >= taille){
                    taille = 2*taille + 256;
                    transo = (transition*)realloc(transo, 2*taille*sizeof(transition));
                    
                }
                int nb_dep = 0;
                for(int i= 0; i < 257; i++){
                    res.first_transition[i] = res.first_transition[i] + nb_dep;
                    if(!(is_in(lettres, i))){
                        nb_dep +=1;
                    }
                }
                nb_dep = 0;
                transition* accumulateur= (transition*)malloc(taille*sizeof(transition));
                transition t1;
                t1.src_state = deb;
                t1.dest_state = fin;
                for(int i=0; i < 256; i++){
                    if(!(is_in(lettres, i))){;
                        for(int indice = res.first_transition[i]; indice < res.first_transition[i+1]-1; indice++){
                            accumulateur[indice] = transo[indice -nb_dep];                          
                        }
                        accumulateur[res.first_transition[i+1]-1] = t1;
                        nb_dep +=1;
                    }
                    else{
                        for(int ind = res.first_transition[i]; ind < res.first_transition[i+1]; ind++){
                            accumulateur[ind] = transo[ind -nb_dep];
                        }
                    }
                }
                copytab(transo, accumulateur, res.first_transition[256]);
                free(accumulateur);
            }
            else{
                acc = fscanf(fichier, " %d;\n",&fin);
                if (deb > max_etat){
                    max_etat = deb;
                }
                if(fin > max_etat){
                    max_etat = fin;
                }
                nb_transi +=256;
                if (nb_transi >= taille){
                    taille = 2*taille + 256;
                    transo = (transition*)realloc(transo, 2*size*sizeof(transition));
                }
                int c_int = c;
                int nb_dep = 0;
                for(int i= 0; i < 257; i++){
                    res.first_transition[i] = res.first_transition[i] + nb_dep;
                    nb_dep += 1;
                }
                nb_dep = 0;                
                transition* accumulateur = (transition*)malloc(taille*sizeof(transition));
                transition t1;
                t1.src_state = deb;
                t1.dest_state = fin;
                for(int i=0; i < 256; i++){
                        for(ind = res.first_transition[i]; ind < res.first_transition[i+1]-1; ind++){ 
                            accumulateur[ind] = transo[ind -nb_dep];
                            char portail = i;
                        }
                        accumulateur[res.first_transition[i+1]-1] = t1;
                        nb_dep +=1;
                }
                copytab(transo, accumulateur, res.first_transition[256]);
                free(accumulateur);
            }
        }
    }
    res.transitions = transo;
    bool* final = (bool*)malloc(max_etat*sizeof(bool));
    for(int i = 0; i < max_etat; i++){
        final[i] = false;
    }
    for(int j = 0; j < nb_winner; j++){
        final[new[j]] = true;
    }
    res.is_final = final;
    res.nb_states = max_etat;
    return res;
}

void test_read(char* filename){
    FILE* fichier = fopen(filename,"r");
    int i;
    char j;
    char* mot = (char*)malloc(256*sizeof(char));
    int k;
    int deb;
    int fin;
    while(feof(fichier)==0){
        int acc = fscanf(fichier, "%d ->[%c] %d;", &i, &j,&k);
        if(acc == 3){
            printf("%d ->[%c] %d\n",i, j, k);
        }
        if(acc == 1){
            //printf("suicide toi et %d\n", i);
            acc = fscanf(fichier, "![%s %d;", mot, &k);
            printf("%s\n", mot);
            if (acc == 2){
                printf("%d ->![%s %d\n", i, mot ,k);
            }
            else{
                acc = fscanf(fichier, " %d;", &k);
                printf("%d -> %d\n", i, k);
            }
        }   
    }

}

int main(){
    transition* toto = (transition*)malloc((6*256)*sizeof(transition));
    int first_transition[1 + 256 * sizeof(unsigned char)];
    int acc = 0;
    int t = 't';
    int o = 'o';
    automaton totomate;
    for (int i = 0; i <= 256; i ++){
        totomate.first_transition[i] = acc;
        
        if (i!=t && i != o){
            for (int j = 0; j <5; j++) {
            transition t1;
            t1.src_state = j;
            if (j != 4){
                t1.dest_state = 0;
                }
            else{
                t1.dest_state = 4;
                }
                toto[acc+j] = t1;
            }
        }
        if(i == t){
            for (int j = 0; j <5; j++) {
            transition t1;
            t1.src_state = j;
            if(j == 0 || j == 2){
                t1.dest_state = j+1;
            }
            if(j == 1){
                t1.dest_state = 1;
            }
            if(j == 3){
                t1.dest_state = 1;
            }
            if(j == 4){
                t1.dest_state = 4;
            }
            toto[acc+j] = t1;
            }
        }
        if(i == o){
            for (int j = 0; j <5; j++) {
            transition t1;
            t1.src_state = j;
            if(j == 0 || j == 2){
                t1.dest_state = 0;
            }
            if(j == 1){
                t1.dest_state = 2;
            }
            if(j == 3 || j == 4){
                t1.dest_state = 4;
            }
            toto[acc+j] = t1;
            }
        }
        acc = acc + 5;
    }
    totomate.transitions = toto;
    totomate.nb_states = 5;
    bool* is_final = (bool*)malloc(5*sizeof(bool));
    is_final[0] = false;
    is_final[1] = false;
    is_final[2] = false;
    is_final[3] = false;
    is_final[4] = true;
    totomate.is_final = is_final;
    if(automaton_accepts(&totomate, "abctoabctoto")){
        printf("Dedans\n");
    }
    else{
        printf("Pas dedans\n");
    }
    char* f = "input.txt";
    //test_read(f);
    
    automaton a =  creer_atomotate(f);
    
    assert(automaton_accepts(&a, "totororo"));
    assert(automaton_accepts(&a, "tttotttotore"));
    assert(automaton_accepts(&a, "adventoftotocode"));
    assert(!(automaton_accepts(&a, "tottottotcode")));
    assert(automaton_accepts(&a, "autotomatotopoto"));
    assert(!automaton_accepts(&a, "EnormeRatiolaquestion6"));
    assert(automaton_accepts(&a, "Je hais le totomate"));
    printf("Succès\n");
    
    
    return 0;
}