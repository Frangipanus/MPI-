#ifndef __TSP_APPROX_H
#define __TSP_APPROX_H

#include <stdbool.h>

struct graphe_complet_pondere_s {
	int sommets;
	int *poids;
};
typedef struct graphe_complet_pondere_s graphe_complet_pondere;

int poids_arete(graphe_complet_pondere *graphe, int u, int v);
int poids_tour(graphe_complet_pondere *graphe, int *tour);
int plus_proche(graphe_complet_pondere *graphe, int sommet, bool *deja_vu);
int *tour_plus_proche(graphe_complet_pondere *graphe, int depart);
int *deux_opt(const int *tour, int sommets, int i, int j);
int *deux_opt_iterative(graphe_complet_pondere *graphe);
void completer_tour_backtrack(graphe_complet_pondere *graphe, int *tour_partiel, int p, int meilleur_tour[], int *poids_meilleur);
int tour_backtrack(graphe_complet_pondere *graphe, int meilleur_tour[]);
//int somme_deux_plus_legeres(const graphe_complet_pondere *graphe, const int *tour_partiel, int p, int v);
//int eval_tour_partiel(const graphe_complet_pondere *graphe, const int *tour_partiel, int p);
//void completer_tour_min(const graphe_complet_pondere *graphe, int *tour_partiel, int p, int *meilleur_tour[], int *poids_meilleur);
//int tour_min(const graphe_complet_pondere *graphe, int *meilleur_tour[]);

#endif /* __TSP_APPROX_H */
