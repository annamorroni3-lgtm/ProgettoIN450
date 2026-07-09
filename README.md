# Progetto IN450

## Titolo

Representative Sets e Partition Tables per GIFT-64

## Autore

Anna Morroni

## Descrizione

Questo progetto implementa l'algoritmo dei Representative Sets descritto nel paper relativo ai differenziali impossibili di GIFT-64.

In particolare il progetto:

- costruisce la tabella **H**, che associa a ogni differenza di output le differenze di input che la possono produrre;
- calcola un **Representative Set** mediante un algoritmo greedy;
- costruisce la **Partition Table** eliminando le sovrapposizioni tra i gruppi;
- ricerca e verifica differenziali impossibili su una versione ridotta della Superbox di GIFT per un numero configurabile di round.

L'implementazione è interamente realizzata in Wolfram Mathematica utilizzando prevalentemente programmazione funzionale.

## Funzioni principali

- `calculateH`
- `greedyRepresentativeSet`
- `makePartitionTable`
- `trovaDifferenzialeImpossibile`
- `verificaDifferenzialeImpossibile`

## Esecuzione

Aprire il file `progettoA4.wl` in Wolfram Mathematica ed eseguire uno dei seguenti comandi.

### Esempio toy

```Mathematica
runToyDemo[]
```

### Esperimento sulla Superbox ridotta

```Mathematica
runGiftSmallExperiment[8]
```

### Ricerca di un differenziale impossibile

```Mathematica
trovaDifferenzialeImpossibile[8, 4]
```

### Verifica di un differenziale impossibile

```Mathematica
verificaDifferenzialeImpossibile[1, 1, 8, 4]
```

## Note

Il progetto implementa una versione ridotta della Superbox di GIFT per consentire la sperimentazione dei Representative Sets, della costruzione delle Partition Tables e della ricerca di differenziali impossibili mantenendo tempi di esecuzione contenuti.
