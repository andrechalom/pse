#include<stdlib.h>
#include<float.h>
#include<math.h>
#define thisV vars[i+(*N)*j]
#define thisR R[i+(*N)*j]

void sw (double *obj, int i, int j, int k) {
	double tmp;
	tmp = obj[i+ k];
	obj[i + k] = obj[j+ k];
	obj[j + k] = tmp;
}
/* Funcao procura a menor correlacao entre as variaveis dadas apos um switch */
void corcorr (double *vars, double *cor, int * N, int * M, int * l, int * FLAGSTOP) {
	double *R;
	int i, j, k, m;
	double mean, sd, sum=0, sq_sum=0;
	/* min* usados para guardar o minimo ateh agora */
	int mini=0, minj=0;
	double minE = 0, E, Tj;
	/* R: Matriz temporaria para guardar os diferentes cenarios. Tj: vetor temporario */
	R = (double *) calloc((*N)*(*M), sizeof (double));
	/* Normaliza as variaveis */
	for (j=0;j< *l;j++) {
		sum=0; sq_sum=0;
		for(i = 0; i < *N; ++i) {
			sum += thisV;
			sq_sum += thisV * thisV;
		}
		mean = sum / (*N);
		sd = sqrt(sq_sum / *N - mean * mean);
		for (i = 0; i< *N; i++)
			thisR = (thisV-mean)/sd;
	}
	/* Calcula o valor de E antes de realizar qualquer troca */
	for (m=0; m < *l-1;m++) {
		Tj = 0;
		for (k=0;k<*N;k++)
			Tj += R[k + (*N) * (*l-1)] * R[k+(*N)*m];
		minE += (Tj/(*N) - cor[(*l-1)+m*(*M)])*(Tj/(*N) - cor[(*l-1)+m*(*M)]);
	}
	for (i =0; i < *N-1; i++) 
		for (j = i+1; j < *N; j++) {
			E =0;
			/* Troca o valor de R[i] e R[j] */
			sw(R, i, j, (*l-1)*(*N));
			for (m=0; m < *l-1;m++) {
				Tj = 0;
				for (k=0;k<*N;k++)
					Tj += R[k + (*N) * (*l-1)] * R[k+(*N)*m];
				E += (Tj/(*N) - cor[(*l-1)+m*(*M)])*(Tj/(*N) - cor[(*l-1)+m*(*M)]);
			}
			/* trabalha com E aqui */
			if (E < minE) {
				mini = i;
				minj = j;
				minE = E;
			}
			/* Finalmente, destroca i e j */
			sw(R, i, j, (*l-1)*(*N));

		}

	if (mini == 0 && minj == 0) {
		*FLAGSTOP = 1;
	} else {
		/* Troca o valor da variavel na posicao i e j e finaliza */
		sw(vars, mini, minj, (*l-1)*(*N));
	}
	free(R);
	return;
}
