#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<stdbool.h>
#include"automata.h"
#include<assert.h>
#include</home/paul/Documents/Spé MPI*/Info/TP/Langage/langage.h>

int NB_CHAR = 256;


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

void automaton_free(automaton *automaton){
    free(automaton->is_final);
    free(automaton->transitions);
    free(automaton);
}

//Les fonctions copytb et is_in ne sont pas demandés. Elles ont été crées dans le but d'avoir un code plus lisible
void copytab(transition* tab1, transition* tab2, int taille){
    for(int j = 0; j < taille; j++){
        tab1[j] = tab2[j];
    }
}

void copytab2(bool* tab1, bool* tab2, int taille){
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

automaton* automaton_read(FILE* fichier){
    int size = 8;
    automaton* res = (automaton*)malloc(sizeof(automaton));
    int* is_final = (int*)malloc(size*sizeof(int));
    for(int j= 0; j < 7; j++){
        fgetc(fichier);
    }
    int ind = 0;
    while(fscanf(fichier, "%d", &is_final[ind])){
        ind = ind + 1;
        if (ind == size){
            int* new = (int*)realloc(is_final, 2*size*sizeof(int));
            is_final = new;
        }
    }
    int nb_winner = ind;
    int* new = realloc(is_final, ind*sizeof(int));
    fgetc(fichier);
    fgetc(fichier);
    int max_etat = 0;
    int taille = 10000;
    transition* transo = (transition*)malloc(taille*sizeof(transition));
    int nb_transi = 0;
    for(int i = 0; i <NB_CHAR+1; i++) {
        res->first_transition[i] = 0;
    }
    int deb=-8;
        int fin;
        unsigned char c;
    while(feof(fichier)==0){        
        int acc = fscanf(fichier, "%d ->[%c] %d;\n", &deb, &c, &fin);
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
            for(int i= c_int+1; i < NB_CHAR+1; i++){
                res->first_transition[i] = res->first_transition[i] + 1;
            }
            for(int j = res->first_transition[NB_CHAR]-1; j >= res->first_transition[c_int+1]; j--){
                transo[j] = transo[j-1];
            }
            transo[res->first_transition[c_int+1]-1] = t1;
        }
        else{
            char* lettres = (char*)malloc(NB_CHAR*sizeof(char));
            int acc = fscanf(fichier, "![%s %d;\n",lettres, &fin);
            
            if(acc == 2){
                
                if (deb > max_etat){
                max_etat = deb;
                }
                if(fin > max_etat){
                    max_etat = fin;
                }
                nb_transi +=NB_CHAR;
                if (nb_transi >= taille){
                    taille = 2*taille + NB_CHAR;
                    transo = (transition*)realloc(transo, 2*taille*sizeof(transition));
                    
                }
                int nb_dep = 0;
                for(int i= 0; i < NB_CHAR+1; i++){
                    res->first_transition[i] = res->first_transition[i] + nb_dep;
                    if(!(is_in(lettres, i))){
                        nb_dep +=1;
                    }
                }
                nb_dep = 0;
                transition* accumulateur= (transition*)malloc(taille*sizeof(transition));
                transition t1;
                t1.src_state = deb;
                t1.dest_state = fin;
                for(int i=0; i < NB_CHAR; i++){
                    if(!(is_in(lettres, i))){;
                        for(int indice = res->first_transition[i]; indice < res->first_transition[i+1]-1; indice++){
                            accumulateur[indice] = transo[indice -nb_dep];                          
                        }
                        accumulateur[res->first_transition[i+1]-1] = t1;
                        nb_dep +=1;
                    }
                    else{
                        for(int ind = res->first_transition[i]; ind < res->first_transition[i+1]; ind++){
                            accumulateur[ind] = transo[ind -nb_dep];
                        }
                    }
                }
                copytab(transo, accumulateur, res->first_transition[NB_CHAR]);
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
                nb_transi +=NB_CHAR;
                if (nb_transi >= taille){
                    taille = 2*taille + NB_CHAR;
                    transo = (transition*)realloc(transo, 2*size*sizeof(transition));
                }
                int nb_dep = 0;
                for(int i= 0; i < NB_CHAR+1; i++){
                    res->first_transition[i] = res->first_transition[i] + nb_dep;
                    nb_dep += 1;
                }
                nb_dep = 0;                
                transition* accumulateur = (transition*)malloc(taille*sizeof(transition));
                transition t1;
                t1.src_state = deb;
                t1.dest_state = fin;
                for(int i=0; i < NB_CHAR; i++){
                        for(ind = res->first_transition[i]; ind < res->first_transition[i+1]-1; ind++){ 
                            accumulateur[ind] = transo[ind -nb_dep];
                        }
                        accumulateur[res->first_transition[i+1]-1] = t1;
                        nb_dep +=1;
                }
                copytab(transo, accumulateur, res->first_transition[NB_CHAR]);
                free(accumulateur);
            }
        }
    }
    res->transitions = transo;
    bool* final = (bool*)malloc(max_etat*sizeof(bool));
    for(int i = 0; i < max_etat; i++){
        final[i] = false;
    }
    for(int j = 0; j < nb_winner; j++){
        final[new[j]] = true;
    }
    res->is_final = final;
    res->nb_states = max_etat+1;
    return res;
}

void test_read(char* filename){
    FILE* fichier = fopen(filename,"r");
    char j;
    char* mot = (char*)malloc(NB_CHAR*sizeof(char));
    while(feof(fichier)==0){
        fscanf(fichier, "%s %c;", mot, &j);
        printf("%s %c\n", mot, j);
        
    }
}


automaton totomate_creer(){
    transition* toto = (transition*)malloc((6*NB_CHAR)*sizeof(transition));
    int acc = 0;
    int t = 't';
    int o = 'o';
    automaton totomate;
    for (int i = 0; i <= NB_CHAR; i ++){
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
    return totomate;
}

automaton* from_local(language* l){ //L'état 0€st létat de depart, puis chaque lettre a pour etat son int + 1
    automaton* res = (automaton*)malloc(sizeof(automaton));
    transition* trans = (transition*)malloc(257*sizeof(transition));
    bool* seen = (bool*)malloc(NB_CHAR*sizeof(bool));
    bool* final = (bool*)malloc((NB_CHAR+1)*sizeof(bool));
    int capa = 257;
    for(int i = 0; i < NB_CHAR+1; i ++){
        final[i] = false;
        res->first_transition[i] = 0;
    }
    for(int i = 0; i < l->last.length; i++){
        final[l->last.members[i]+1] = true;
    }
    if(l->has_empty){
        final[0] = true;
    }
    res->is_final = final;
    int nb_dep = 1;
    for(int i = 0; i < l->first.length-1; i++){
        transition t1;
        t1.src_state = 0;
        t1.dest_state = l->first.members[i]+1;
        for(int j = l->first.members[i]+1; j <= l->first.members[i+1]; i++){
            res->first_transition[j] = res->first_transition[j] + nb_dep;
        }
        trans[res->first_transition[l->first.members[i]]] = t1;
        nb_dep++;
    }
    transition t1;
    t1.src_state = 0;
    t1.dest_state = l->first.members[l->first.length]+1;
    trans[res->first_transition[l->first.members[l->first.length]]] = t1;
    for(int j = l->first.members[l->first.length-1]+1; j < NB_CHAR; j++){
        res->first_transition[j] = res->first_transition[j]+nb_dep;
    }
    for(int i = 0; i < l->first.length; i++){
        seen[l->first.members[i]] = true;
    }
    for (int j = 0; j < 256; j ++){
        int ind = res->first_transition[j+1];
        
        for(int k = j+1; k < NB_CHAR+1; k++){
            res->first_transition[k] = res->first_transition[k] + l->allowed[k].length;
        }
        if(res->first_transition[256] > capa){
            capa = 2*capa;
            trans  = (transition*)realloc(trans, 2*capa*sizeof(transition));
        }
        for (int m = 0; m < l->allowed[j].length; m ++){
            transition t;
            t.src_state = j+1;
            t.dest_state = l->allowed[j].members[m]+1;
            trans[ind+m] = t;
        }
    }
    res->transitions = trans;
    res->nb_states = 257;
    return res;
}

bool automaton_nd_accepts_from(int state, const automaton* a, const char* word){
    if(word[0]=='\0'){
        return a->is_final[state];
    }
    for(int i = a->first_transition[word[0]]; i < a->first_transition[word[0]+1]; i++){
        if(a->transitions[i].src_state == state){
            if(automaton_nd_accepts_from((a->transitions[i].dest_state), a, &(word[1]))){
                return true;
            }
        }
    }
    return false;
}

bool automaton_nd_accepts(const automaton *automaton, unsigned const char *word){
    automaton_nd_accepts_from(0, automaton, word);
}


bool automaton_nd_accepts_parallel(const automaton *automate, unsigned const char *word){
    bool* state = (bool*)malloc(automate->nb_states*sizeof(bool));
    for(int i = 0; i < automate->nb_states; i++){
        state[i] = false;
    }
    state[0] = true;
    int ind  = 0;
    while(word[ind] != '\0'){
        bool* state2 = (bool*)malloc(automate->nb_states*sizeof(bool));
        for(int i = 0; i < automate->nb_states; i++){
            state2[i] = false;
        }
        for(int i = automate->first_transition[word[ind]]; i < automate->first_transition[word[ind]+1]; i++){
            if (state[automate->transitions[i].src_state]){
                state2[automate->transitions[i].dest_state] = true;
            }
        }
        copytab2(state, state2, automate->nb_states);
        free(state2);    
        ind ++;
    }
    for(int i = 0; i < automate->nb_states; i++){
        if(state[i] && automate->is_final[i]){
            return true;
        }
        
    }
    return false;
}


void test(char* f1, char* f2){
    FILE* F1 = fopen(f1,"r");
    automaton* testeur = automaton_read(F1);
    FILE* F2 = fopen(f2,"r");
    char* mot = (char*)malloc(NB_CHAR*sizeof(char));
    char c;
    while(feof(F2)==0){
        int acc = fscanf(F2, "%s %c;", mot, &c);
        if (c == 'T'){
            assert(automaton_nd_accepts_parallel(testeur,mot));
        }
        else{
            assert(!automaton_nd_accepts_parallel(testeur,mot));
        }
    }
    printf("Success!\n");
}

int count(char* f1, char* f2){
    int cmp = 0;
    FILE* F1 = fopen(f1,"r");
    automaton* testeur = automaton_read(F1);
    FILE* F2 = fopen(f2,"r");
    char* mot = (char*)malloc(NB_CHAR*sizeof(char));
    for(int i = 0; i < testeur->nb_states+1; i++){
        if(testeur->is_final[i]){
            printf("%d\n", i);
        }
    }
    while(feof(F2)==0){
        int acc = fscanf(F2, "%s;", mot);
        if(automaton_accepts(testeur, mot)){
            cmp +=1;
        }
    }
    return cmp;
}



int main(int argc, char *argv[]){
    char*f = (char*)malloc(50*sizeof(char));;
    if(argc>3){
        f = argv[1];
        char* f2 = argv[2];
        char* f3 = argv[3];
        if(f3[0] == 'f'){
            
            test(f, f2);
        }
        if(f3[0] == 'w'){
            FILE* F = fopen(f, "r");
            automaton* a = automaton_read(F);
            
            if(automaton_accepts(a, f2)){
                printf("Le mot est reconnu par l'automate\n");
            }
            else{
                printf("Le mot n'est pas reconnu par l'automate\n");
            }
        }
        if(f3[0] =='c'){
            int nb_dedans = count(f, f2);
            printf("Le fichier %s contient %d mots de l'automate décrit par le fichier %s\n", f2, nb_dedans, f);
        }
    }
    else{
        f = "input.txt";
    }
    //automaton toto = totomate_creer();
    //bool acc = automaton_nd_accepts_parallel(&toto, "rototo");
    //if(acc){
    //    printf("here");
    //}
    return 0;
}