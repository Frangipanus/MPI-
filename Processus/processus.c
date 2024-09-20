#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<assert.h>
#include<stdbool.h>
#include<pthread.h>
#include<time.h>
#include"processus.h"

int acc;
int* compteur = &acc;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t actions1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t actions2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t avant_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t apres_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t pivot_indice = PTHREAD_MUTEX_INITIALIZER;
int capa_init = 16;



void* incremente(void* v){
    for(int i=0; i<10000; i++){
        pthread_mutex_lock(&mutex);
        compteur[0] = compteur[0]+1;
        pthread_mutex_unlock(&mutex);
        
    }
    return v;
}

void* lutins(void* v_untyped){
    
    argument* v = v_untyped;
    int i = v->ind;
    int ma_phase = i % 2;
    while(v->nb_action_ind < v->taille){
        if(v->phase[0] % 2 == (i % 2) && !(v->a_agi[0])){
            if(v->tab[i]>v->tab[i+1]){
                int acc = v->tab[i];
                v->tab[i] = v->tab[i+1];
                v->tab[i+1] = acc;
            }
            v->nb_action_ind = v->nb_action_ind + 1;
            if(i%2==0){
                pthread_mutex_lock(&actions1);
            v->nb_action[0] = v->nb_action[0]+1;
            
            pthread_mutex_unlock(&actions1);
            v->a_agi[0]= true;
            }
            else{
                pthread_mutex_lock(&actions2);
            v->nb_action[0] = v->nb_action[0]+1;
            
            pthread_mutex_unlock(&actions2);
            v->a_agi[0]= true;
            }
        }
        else{
            if(ma_phase < v->phase[0]){
                ma_phase = ma_phase + 2;
                v->a_agi[0] = false;
            }
        }
    }
    return v_untyped;
    
}




void tri_bulle(int length, int* tableau){
    pthread_t* tthreadtab = (pthread_t*)malloc(length*sizeof(pthread_t));
    argument* tabarg = (argument*)malloc(length*sizeof(argument));
    int* action = (int*)malloc((sizeof(int)));
    int* action1 = (int*)malloc((sizeof(int)));
    int* action2 = (int*)malloc((sizeof(int)));
    int* phase = (int*)malloc((sizeof(int)));
    bool** vas_y = (bool**)malloc(length*sizeof(bool*));
    for(int j = 0; j < length; j ++){
        bool* acc = (bool*)malloc(sizeof(bool));
        vas_y[j] = acc;
    }
    action[0]=0;
    action1[0]=0;
    action2[0]=0;
    phase[0]=0;
    for(int j=0; j<length-1; j++){
        vas_y[j][0] = false;
        tabarg[j].ind = j;
        tabarg[j].tab = tableau;
        tabarg[j].taille = length;
        tabarg[j].nb_action_ind = 0;
        if(j%2 == 0){
            tabarg[j].nb_action = action1;
        }
        else{
            tabarg[j].nb_action = action2;
        }
        tabarg[j].phase = phase;
        tabarg[j].a_agi = vas_y[j];
    }
    
    for(int i = 0; i < length-1; i++){
        void* v = &tabarg[i];
        pthread_create(&tthreadtab[i], NULL, lutins, v);        
    } 
    while(phase[0]< 2*length){
        //printf("On est à la phase %d et %d  %d\n", phase[0], tabarg[1].nb_action_ind, action2[0]);
        if(phase[0]%2 == 0){
            while(action1[0]<length/2){   
                ;
            }
            phase[0] = phase[0] + 1;
            pthread_mutex_lock(&actions1);
            action1[0] = 0;
            pthread_mutex_unlock(&actions1);
        }
        else{
          while(action2[0]<(length-1)/2 ){
                ;
            }
            phase[0] = phase[0] + 1;  
            pthread_mutex_lock(&actions2);
            action2[0] = 0;
            pthread_mutex_unlock(&actions2);
        }

    }
    for(int i = 0; i < length-1; i++){
        pthread_join(tthreadtab[i], NULL);
    }   
}


void* partitionne(void* v){
    quick* q = (quick*)v;
    int pivot = q->tab[q->fin -1];
    int ind = q->debut;
    if(q->fin - q->debut <= 1){
        return v;
    }
    for(int i = q->debut; i<= q->fin-1; i++){
        if(q->tab[i]<= pivot){
            int acc = q->tab[i];
            q->tab[i] = q->tab[ind];
            q->tab[ind] = acc;
            ind = ind + 1;
        }
    }
    q->tab[q->fin-1] = q->tab[ind];
    q->tab[ind] = pivot;
    //On a partitionner autour du pivot
    pthread_t avant;
    pthread_t apres;
    quick* q1 = (quick*)malloc(sizeof(quick));
    quick* q2 =  (quick*)malloc(sizeof(quick));
    q1->tab = q->tab;
    q2->tab = q->tab;
    q1->debut = q->debut;
    q1->fin = ind;
    q2->debut = ind;
    q2->fin = q->fin;
    void* v1 = q1;
    void* v2 = q2;
    pthread_create(&avant, NULL, partitionne, v1);
    pthread_create(&apres, NULL, partitionne, v2);
    pthread_join(avant, NULL);
    pthread_join(apres, NULL);
    return v;
}




void tri_para(int length, int* tab){
    quick* q = (quick*)malloc(sizeof(quick));
    q->debut = 0;
    q->fin = length;
    q->tab = tab;
    void* v = (void*)q;
    pthread_t trie;
    pthread_create(&trie, NULL, partitionne, v);
    pthread_join(trie, NULL);
}

int compareFunction( const void * ptr1, const void * ptr2 ){
    int* v = ptr1;
    int* v2 = ptr2;
    return(v[0]-v2[0]);
}



void* partitionne2(void* v){
    quick2* q = v;
    int pivot = q->pivot[0];
    int ind = q->debut;
    if(q->fin  - q->debut < 1){
        return v;
    }
    for(int i = q->debut; i< q->fin; i++){
        //printf("%d\n",q->tab[i]);
        if(q->tab[i]<= pivot){
            //printf("ind = %d, tab = %d, pivot = %d\n",i,q->tab[i],pivot);
            int acc = q->tab[i];
            q->tab[i] = q->tab[ind];
            q->tab[ind] = acc;
            ind = ind + 1;
        }
    }
    //printf("ind = %d\n", ind);
    pthread_mutex_lock(&avant_mutex);
    int av = q->avant[0];
    q->avant[0] = q->avant[0] + ind - q->debut;
    
    //printf("Le processuc stocke jusqu'a%d\n", q->avant[0]);
    pthread_mutex_unlock(&avant_mutex);
    for(int i = 0; i < ind-q->debut; i++) {
        q->acc[av+i] = q->tab[q->debut + i];
        
        //printf("%d\n", q->acc[av+i]);
        //printf("hey\n");
    }
    pthread_mutex_lock(&apres_mutex);
    int ap = q->apres[0];
    q->apres[0] = q->apres[0]- (q->fin - ind);
    //printf("Le processuc stocke jusqu'a%d\n", q->apres[0]);
    pthread_mutex_unlock(&apres_mutex);
    for(int i = q->fin-1; i>=ind; i--) {
        //printf("hey");
        q->acc[ap - (i-ind)] = q->tab[i];
        
    }
    return v;


}

void tri_efficace(int length, int* tab, int nb_proccess, int debut){
    //printf("Debut = %d\n", debut);
    if(length == 1){
        return;
    }
    if(nb_proccess == 1 || nb_proccess <=length){
        //printf("le début des soucis, %d, debut = %d\n", length, debut);
        qsort(&(tab[debut]), length, sizeof(int),compareFunction);
        //printf("la fin des soucies\n");
        //tri_para(length, &tab[debut]);
        return;
    }
    
    
    int* deb = (int*)malloc(sizeof(int)); 
    int* fin = (int*)malloc(sizeof(int));
    int* acc = (int*)malloc(length*sizeof(int));
    int* pivot = (int*)malloc(length*sizeof(int));
    pivot[0]= tab[debut];
    tab[debut] = tab[debut + length - 1];
    tab[debut + length - 1] = pivot[0];
    deb[0] = 0;
    fin[0] = length-1;
    bool* found = (bool*)malloc(sizeof(bool));
    found[0]= false;
    int* ind_pv  = (int*)malloc(sizeof(int));
    ind_pv[0]= debut+length-1;
    //printf("eher");
    quick2* tabarg = (quick2*)malloc(nb_proccess*sizeof(quick2));
    int taille = length / nb_proccess;
    //printf("pivot = %d\n",pivot[0]);
    for(int i=0; i<nb_proccess-1; i++){
        tabarg[i].tab = tab;
        tabarg[i].acc = acc;
        tabarg[i].debut = i*taille;
        tabarg[i].fin = (i+1)*(taille);
        tabarg[i].avant = deb;
        tabarg[i].apres = fin;
        tabarg[i].pivot = pivot;
        tabarg[i].pivot_found = found;
        tabarg[i].ind_pivot = ind_pv;
    }
    tabarg[nb_proccess -1].tab = tab;
    tabarg[nb_proccess -1].acc = acc;
    tabarg[nb_proccess -1].debut = (nb_proccess-1)*taille;
    tabarg[nb_proccess -1].fin = debut + length;
    tabarg[nb_proccess -1].avant = deb;
    tabarg[nb_proccess -1].apres = fin;
    tabarg[nb_proccess -1].pivot = pivot;
    tabarg[nb_proccess -1].ind_pivot = ind_pv;
    pthread_t* pross = (pthread_t*)malloc(nb_proccess*sizeof(pthread_t));
    for (int i = 0; i < nb_proccess; i++){
        pthread_create(&(pross[i]), NULL,partitionne2, &tabarg[i]);
    }
    for (int i = 0; i < nb_proccess; i++){
        pthread_join((pross[i]), NULL);
    }
    int ind_max = debut;
    for(int i = debut; i < debut + length; i++){
        //printf("%d\n", acc[i-debut]);
        tab[i] = acc[i-debut];
        if(tab[i] == pivot[0]){
            ind_pv[0] = i;
        }
        if(tab[i]<=pivot[0]){
            ind_max = i;
        }
    }
    tab[ind_pv[0]]=tab[ind_max];
    tab[ind_max]=pivot[0];
    

    int milieu = ind_max+1;
    printf("milieu = %d début = %d, length = %d, pivot = %d\n", milieu, debut, length , pivot[0]);
    tri_efficace(milieu-debut,tab,nb_proccess/2,debut);
    tri_efficace(length + debut - milieu,tab,nb_proccess - nb_proccess/2, milieu);
}



deque* deque_new(){
    deque* res = (deque*)malloc(sizeof(deque));
    res->capacity = capa_init;
    res->top = 0;
    res->botom = 0;
    void** data = (void**)maloc(capa_init*sizeof(void*));
}

bool deque_in_empty(deque* f){
    int bot = f->botom;
    return(bot+1 != f->top );
}

void deque_resize(deque* f){
    //pthread_mutex_lock(&f->top_lock);
    void** data = (void**)malloc((f->capacity * 2)*sizeof(void*));
    for(int i = 0; i< f->capacity; i ++){
        data[i] = f->data[(i+f->top)%(f->capacity)];
    }
    free(f->data);
    f->data = data;
    f->botom = f->capacity-1;
    f->top = 0;
    f->capacity = f->capacity * 2;
    //pthread_mutex_unlock(&f->top_lock);
}

void deque_poptop(deque* f){
    pthread_mutex_lock(&f->top_lock);
    if(is_empty(f)){
        return NULL;
    }
    void* res = f->data[f->top];
    f->top = f->top + 1;
    pthread_mutex_unlock(&f->top_lock);
    return(res);
}

void push_bottom(deque* f, void* x){
    pthread_mutex_lock(&f->top_lock);
    int top = f->botom + 1;
    f->botom = top;
    if(top = f->top){
        f->data[top] =  x;
        deque_resize(f);
        pthread_mutex_unlock(&f->top_lock);
    }
    else{
        pthread_mutex_unlock(&f->top_lock);
        f->data[top] = x;
    }
    


}


int main(void){
    srand(time(NULL));
	pthread_t Alice;
    pthread_t Bob;
    pthread_create(&Alice, NULL, incremente, NULL);
    pthread_create(&Bob, NULL, incremente, NULL);
    pthread_join(Alice, NULL);
    pthread_join(Bob, NULL);
    printf("On obtient %d\n", compteur[0]);
    int* tableau = (int*)malloc(10*sizeof(int));
    tableau[0] = 5;
    tableau[1] = 4;
    tableau[2] = 7;
    tableau[3] = 8;
    tableau[4] = 15;
    tableau[5] = 3;
    tableau[6] = 9;
    tableau[7] = 12;
    tableau[8] = 75;
    tableau[9] = 6;
    //tri_para(10,tableau);
    //qsort(&(tableau[5]),5, sizeof(int),compareFunction);
    //for(int i = 0; i < 10; i++){
    //    printf("%d\n", tableau[i]);
    //}
    const int length = 50000000;
    const int nb_pross = 16;
    //printf("----------------------------------------------------------------\n");
    
    int* tableau2 = (int*)malloc(length*sizeof(int));
    
    for(int i = 0; i <length; i++){
        tableau2[i] = rand()%1000;
        //printf("%d\n", tableau2[i]);
    }
    
    

    //tri_bulle(length,tableau2);
    
    tri_efficace(length, tableau2, nb_pross, 0);
    //printf("End géné\n");
    //qsort(tableau2,length,sizeof(int), compareFunction);
    //for(int i = 0; i < length; i++){
    //    //printf("%d\n", tableau2[i]);
    //}
    //for(int i = 0; i < length-1; i++){
    //    if(tableau2[i+1]<tableau2[i]){
    //        printf("Perdu\n");
    //        return 0;
    //    }
    //}
    //printf("Sucess\n");
    
    

    return 0;
    
}