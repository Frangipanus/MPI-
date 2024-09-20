#include<stdlib.h>
#include<stdio.h>
#include"tsp_approx.h"
#include<stdbool.h>


int poids_arete(graphe_complet_pondere *graphe, int u, int v){
    return graphe->poids[u*graphe->sommets + v];
}

int poids_tour(graphe_complet_pondere* g, int* sommets){
    int poid = 0;
    for(int i = 0; i < g->sommets; i ++){
        poid = g->poids[g->sommets*sommets[i] + sommets[(i+1) % g->sommets]] + poid;
    }
    return poid;
}

int plus_proche(graphe_complet_pondere* g, int sommet, bool* deja_vu){
    int min_poids = -1;
    int min_som = 0;
    for(int i = 0; i < g->sommets; i++){
        if (!(deja_vu[i]) && ((g->poids[sommet*g->sommets + i ] < min_poids)|| (min_poids == -1)) ){
            min_poids = g->poids[sommet*(g->sommets) + i];
            min_som = i;
        }
    }
    return min_som;
}

void __print_tour(int len, int* tour){
    for(int i = 0; i < len; i ++){
        printf("%d --> ", tour[i]);
    }
    printf("\n");
}

int* tour_plus_proche(graphe_complet_pondere* g, int depart){
    int* tour = (int*)malloc(g->sommets * sizeof(int));
    bool* deja_vu = (bool*)malloc(g->sommets * sizeof(bool));
    for(int i = 0; i < g->sommets;i++){
        deja_vu[i] = false;
    }
    int som = depart;
    for(int i = 0; i < g->sommets; i++){
        som = plus_proche(g, som,deja_vu);
        deja_vu[som] = true;
        tour[i] = som;
        
    }
    return tour;
}

int* deux_opt(const int* tours,int sommet, int i, int j){
    int* res = (int*)malloc(sommet*sizeof(int));
    for(int k = 0; k <= i; k++){
        res[k] = tours[k];
    }
    int cmp = i+1;
    for(int ind = j; ind > i; ind--){
        res[cmp] = tours[ind];
        cmp = cmp+1;
    }
    res[j] = tours[(i+1)%sommet];
    for(int ind = j+1; ind < sommet; ind++){
        res[ind] = tours[ind]; 
    }
    return res;
}


void __copy_tab(int* t1, int* t2, int len){
    for(int i = 0; i < len; i ++){
        t1[i] = t2[i];
    }
}
 
int* deux_opt_iterative(graphe_complet_pondere*  g){
    int* tour = tour_plus_proche(g, 0);
    print_tour(g->sommets, tour);
    printf("%d\n", poids_tour(g, tour));
    bool possible = true;
    int poid = poids_tour(g,tour);
    while (possible){
        possible = false;
        for(int i = 0; i < g->sommets-2; i++){
            for(int j = i+2; j < g->sommets; j++){
                int* acc = deux_opt(tour, g->sommets, i, j);
                //printf("%d, %d " ,i,j);
                //print_tour(g->sommets, acc);
                if(poids_tour(g, acc) < poid){
                    
                    __copy_tab(tour, acc, g->sommets); //A ameliorer
                    poid = poids_tour (g,acc);
                    print_tour(g->sommets, tour);
                    possible = true;
                    break;
                }
            }
            if(possible){
                break;
            }
        }
    }
    return tour;
}

void completer_tour_backtrack(graphe_complet_pondere *graphe, int *tour_partiel, int p, int meilleur_tour[], int *poids_meilleur){
    if(p == graphe->sommets){
        int poid = poids_tour(graphe, tour_partiel);
        if (poid < poids_meilleur[0]){
            for(int i = 0; i < p; i ++){
                meilleur_tour[i] = tour_partiel[i];
                poids_meilleur[0] = poid;  
            }
        }
    }
    else{
        bool* visites = (bool*)malloc((graphe->sommets*sizeof(bool)));
        for(int i = 0; i < graphe->sommets; i++){
            visites[tour_partiel[i]] = false;
        }
        for(int i = 0; i < p; i++){
            visites[tour_partiel[i]] = true;
        }
        for (int j = 0; j < graphe->sommets; j ++){
            if(!(visites[j])){
                tour_partiel[p] = j;
                completer_tour_backtrack(graphe, tour_partiel, p+1, meilleur_tour, poids_meilleur);
            }
        }
        free(visites);
    }
}

int tour_backtrack(graphe_complet_pondere *graphe, int meilleur_tour[]){
    int* tour_partiel = (int*)malloc((graphe->sommets)*sizeof(int));
    int* poids_max = (int*)malloc(sizeof(int));
    poids_max[0] = poids_tour(graphe, meilleur_tour);
    completer_tour_backtrack(graphe, tour_partiel, 0, meilleur_tour, poids_max);
    free(tour_partiel);
    int res = poids_max[0];
    free(poids_max);
    return res;
}

//int main(void){
//    return 0;
//}