#include <stdio.h>
#include <stdbool.h>
// 1. Type Safety

void endiannessValidation(){
	unsigned int intero = 1;
	char *c = (char*)&intero;
	if(*c == 1){
		printf("Little Endian system");
	}else{
		printf("Big Endian system");
	}
}

// Il programma su un processore Apple M2 da risultato: Little Endian
// Il programma su un processore AMD da risultato: Little Endian

//

// 2. Array Mutabili

struct Nodo {
    int valore;
    struct Nodo* left;
    struct Nodo* right;
};

struct Punto {
    int valore;
    struct Punto* next;
};

bool duplicato = false; // Variabile globale che ci indica se l'elemento inserito è un duplicato o no

struct Punto* creaPunto(int val);
struct Nodo* creaNodo(int val);
struct Nodo* inserisci(struct Nodo* nodo, int val);

struct Punto* removeDups(int array[], int dim) {
    if (dim == 0) return NULL;
    int i = 0;
    struct Punto* head = creaPunto(array[i]);
    struct Punto* tail = head;
    struct Nodo* testa = creaNodo(array[i++]);

    for (i; i < dim; i++) {
        testa = inserisci(testa, array[i]);
        if (!duplicato) {
            tail->next = creaPunto(array[i]);
            tail = tail->next;
        }
        duplicato = false;
    }

    return head;
}

struct Nodo* creaNodo(int val) {
    struct Nodo* nodo = (struct Nodo*)malloc(sizeof(struct Nodo));
    nodo->valore = val;
    nodo->left = NULL;
    nodo->right = NULL;
    return nodo;
}

struct Punto* creaPunto(int val) {
    struct Punto* lista = (struct Punto*)malloc(sizeof(struct Punto));
    lista->valore = val;
    lista->next = NULL;
    return lista;
}

struct Nodo* inserisci(struct Nodo* nodo, int val) {
    if (nodo == NULL) { // caso base
        return creaNodo(val);
    }
    if (val == nodo->valore) {
        // Elemento già presente nell'albero binario, quindi è un duplicato.
        duplicato = true;
        return nodo;
    }
    if (val < nodo->valore) { // valore in input minore del valore nel nodo, vado a SX
        nodo->left = inserisci(nodo->left, val);
    } else { // valore in input maggiore del valore nel nodo, vado a DX
        nodo->right = inserisci(nodo->right, val);
    }
    return nodo;
}

// Il costo di questa funzione e' O(m log n), dove m e' la lunghezza dell'array e n e' il numero di nodi dell'albero binario
// nel caso peggiore, dove l'array e' gia ordinato, la complessita' e' O(n^2)


// 3. Quello che in Haskell non si puo' fare! pt I

typedef struct cBinTree {
    int k;
    int n;
    int risultato;
    struct cBinTree* left;
    struct cBinTree* right;
} cBinTree;

struct cBinTree* creaNodoCBin(int n, int k);
struct cBinTree* cbin(int n, int k);
cBinTree* matrice[50][50] = {NULL};

struct cBinTree* creaNodoCBin(int n, int k){
	struct cBinTree* nodo = (struct cBinTree*)malloc(sizeof(struct cBinTree));
	nodo->k = k;
    nodo->n = n;
    nodo->risultato = NULL;
    nodo->left = NULL;
	nodo->right = NULL;
	return nodo;
}

//void cbinfun(int n, int k){
	//cBinTree* matriceNodi[n][k] = {NULL};
	//cbin(n,k,matriceNodi);
//}

struct cBinTree* cbin(int n, int k){
	if(matrice[n][k] == NULL){
		if (n==k || n==0 || k == 0){		
			struct cBinTree* nodo = creaNodoCBin(n,k);
	    	nodo->risultato = 1;
	    	printf("Nodo foglia lv:%d = n:%d k:%d r:%d\n", n,n,k,1);
	    	return nodo;
	    	
		} 
			struct cBinTree* nodo = creaNodoCBin(n,k);
			matrice[n][k] = nodo;
	    	nodo -> left =  cbin(n-1,k-1);
			nodo -> right = cbin(n-1,k);
			nodo-> risultato =  (nodo -> left -> risultato + nodo -> right -> risultato);	
			printf("Nodo lv:%d = n:%d k:%d r:%d\n", n,n,k,nodo-> risultato);
			return nodo;
		
	}else{
		return matrice[n][k];
	}
}

// 4. Quello che in Haskell non si puo' fare! pt II



 


// TEST AREA 

int main() {
	// ES. 1
		//endiannessValidation();
	//
	
	// ES. 2
		//int array[] = {3, 3, 3, 5, 3, 2, 5, 6, 7, 5, 3, 4, 6, 8, 9, 3, 21, 4, 5, 4, 6, 7, 5};
	    //int dim = sizeof(array) / sizeof(array[0]);
	//	struct Punto* lista = removeDups(array, dim);
		//while (lista != NULL) {
	     //   printf("%d ", lista->valore);
	    //    lista = lista->next;
	   // }
	//
	
	// ES. 3
		cbin(5,3);
}



