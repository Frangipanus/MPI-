#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "tsp_approx.h"

int main(int argc, char *argv[])
{
	
	int poids_graphe_exemple[] = {
		0, 1, 10, 5, 4,
		1, 0, 6, 12, 7,
		10, 6, 0, 2, 3,
		5, 12, 2, 0, 9,
		4, 7, 3, 9, 0
	};
	graphe_complet_pondere graphe_exemple = {
		.sommets = 5,
		.poids = poids_graphe_exemple
	};
	assert(graphe_exemple.sommets * graphe_exemple.sommets == sizeof(poids_graphe_exemple)/sizeof(int));
	
	int tour[] = {0, 2, 3, 1, 4};

	printf("Le tour a pour poids %d\n", poids_tour(&graphe_exemple, tour));
	printf("Le poids minimum avec la première méthode est %d\n", poids_tour(&graphe_exemple, deux_opt_iterative(&graphe_exemple)));
	printf("Le meilleur cycle à comme poids %d\n", tour_backtrack(&graphe_exemple, tour));
	return 0;
}
