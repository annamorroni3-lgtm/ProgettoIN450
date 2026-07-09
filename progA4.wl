(* ::Package:: *)

(* ::Package:: *)
(* ========================================================= *)
(*  Progetto: Greedy Representative Sets per GIFT-64          *)
(**)
(* ========================================================= *)


(*Progetto A4. GIFT-64: differenziali impossibili mediante representative sets *)

ClearAll[mask, xor, convertiHexElemento];
ClearAll[dividiNibble, unisciNibble];

mask[n_Integer] := 2^n - 1;

xor[a_Integer, b_Integer] := BitXor[a, b];

dividiNibble[x_, nNibble_] :=
  Map[
    Function[pos, BitAnd[BitShiftRight[x, 4 pos], 15]],
    Range[0, nNibble - 1]
  ];

unisciNibble[nibbles_] :=
  Total[
    MapIndexed[
      Function[{val, idx}, BitShiftLeft[val, 4 (First[idx] - 1)]],
      nibbles
    ]
  ];
convertiHexElemento[numeroDaConvertire_Integer, numeroCifreEsadecimali_Integer] :=
  IntegerString[numeroDaConvertire, 16, numeroCifreEsadecimali];


(* --------------------------------------------------------- *)
(*  S-box di GIFT-64                                         *)
(* --------------------------------------------------------- *)
ClearAll[sboxGift, stratoS];

sboxGift = {1,10,4,12,6,15,3,9,2,13,11,7,5,0,8,14};

stratoS[x_, nNibble_] :=
  unisciNibble[
    Map[
      Function[val, sboxGift[[val + 1]]],
      dividiNibble[x, nNibble]
    ]
  ];




(* --------------------------------------------------------- *)
(*  Permutazioni P1 e P2                                     *)
(* --------------------------------------------------------- *)
ClearAll[permutaNibble, P1, P2];

P1 = {0,5,10,15,12,1,6,11,8,13,2,7,4,9,14,3};
(*P2 = {0,4,8,12,1,5,9,13,2,6,10,14,3,7,11,15};
P2 non viene applicata perch\[EAcute], essendo lineare e invertibile,
   il paper mostra che pu\[OGrave] essere omessa nell'analisi differenziale. *)
permutaNibble[x_, permutazione_] :=
  Module[
    {nibbleOriginali, nibblePermutati},

    nibbleOriginali =
      dividiNibble[x, Length[permutazione]];

    nibblePermutati =
      Part[nibbleOriginali, permutazione + 1];

    unisciNibble[nibblePermutati]
  ];



(* --------------------------------------------------------- *)
(*  Superbox GIFT                                            *)
(* --------------------------------------------------------- *)
ClearAll[superboxGift, superboxRidotta];

superboxGift[x_] :=
  stratoS[
    permutaNibble[
      stratoS[x, 16],
      P1
    ],
    16
  ];

superboxRidotta[nBits_Integer] :=
  Function[
    {x},
    BitAnd[superboxGift[x], mask[nBits]]
  ];


(* --------------------------------------------------------- *)
(*  Algorithm 3: calculate_H                                 *)
(* --------------------------------------------------------- *)

ClearAll[AllDiffInput, allInputPairs, differentialPair, calculateH];

AllDiffInput[numeroDiBit_Integer] :=
  Range[1, 2^numeroDiBit - 1];
(*\[EGrave] l'insieme di tutte le differenze di input possibili. es: AllDiffInput[8]=Range[0, 2^8 - 1]=Range[0,255]={0,1,2,...,255}*)

allInputPairs[size_Integer] :=
  Tuples[
    {
      Range[0, size - 1],
      Range[1, size - 1]
    }
  ];
(* Genera tutte le coppie {x, \[Alpha]} da analizzare.
   x \[EGrave] un possibile valore di input,
   \[Alpha] \[EGrave] una possibile differenza di input (esclusa 0). 
    f(x) \[CirclePlus] f(x \[CirclePlus] \[Alpha]). *)
   
differentialPair[f_, pair_List] :=
  Module[
    {x, alpha, y},

    x = pair[[1]];
    alpha = pair[[2]];

    y =
      xor[
        f[x],
        f[xor[x, alpha]]
      ];

    {y, alpha}
  ];
(*esegue y=f(x)\[CirclePlus]f(x\[CirclePlus]\[Alpha]) e poi restituisce {y, \[Alpha]}*)

calculateH[f_, size_Integer] :=
  GroupBy[
    Map[
      Function[{pair}, differentialPair[f, pair]],
      allInputPairs[size]
    ],
    First -> Last,
    DeleteDuplicates
  ]; (*raggruppa usando il primo elemento come chiave, e conserva il secondo elemento come valore.*)






(* --------------------------------------------------------- *)
(*  Algorithm 3: scelta greedy                               *)
(* --------------------------------------------------------- *)

ClearAll[
  coveredByKey,
  chooseBestKey,
  greedyStep,
  greedyFinishedQ,
  greedyRappresentanti
];

coveredByKey[H_Association, uncovered_List, key_Integer] :=
  Intersection[H[key], uncovered]; (*coveredByKey restituisce le differenze ancora non coperte che possono essere coperte dal rappresentante key*)


chooseBestKey[H_Association, uncovered_List] :=
  Module[
    {keys = Keys[H], scores, nonEmpty}, (*Keys[H] restituisce tutte le chiavi dell'Association*)

    scores =
      Map[
        Function[{key},
          {key, coveredByKey[H, uncovered, key]} (*quali differenze ancora scoperte riesce a coprire.*)
        ],
        keys
      ];

    nonEmpty = Select[scores, Length[Last[#]] > 0 &]; (*tieni solo i rappresentanti che coprono almeno una differenza.*)

    If[
      nonEmpty === {},
      Null,
      First[  (*prende semplicemente il primo elemento della lista.*)
        SortBy[
          nonEmpty,
          Function[{pair}, -Length[Last[pair]]] (*ordina in base a quante differenze coprono*)
        ]
      ][[1]] (*Prende il primo elemento della coppia, cio\[EGrave] il rappresentante migliore*)
    ]
  ];
  
  (*chooseBestKey ordina tutti i rappresentanti utili in base al numero di differenze ancora scoperte che coprono, prende il primo della lista e restituisce la sua chiave.*)


greedyStep[H_Association, state_Association] :=
  Module[
    {key, newlyCovered},

    key = chooseBestKey[H, state["Uncovered"]];  (*es. chooseBestKey[H, {4,5,6}]*)

    If[
      key === Null,
      state,
      newlyCovered = coveredByKey[H, state["Uncovered"], key]; (*calcola quali differenze copre key*)

      <|
        "S" -> Append[state["S"], key], (*aggiunge key a S*)
        "Uncovered" -> Complement[state["Uncovered"], newlyCovered] (*rimuove quelle differenze da Uncovered.*)
      |>
    ]
  ];


greedyFinishedQ[state_Association] :=
  state["Uncovered"] == {};


(*H \[EGrave] la tabella che hai appena costruito con calculateH;
X \[EGrave] l'insieme di tutte le differenze da coprire.*)
greedyRappresentanti[H_Association, X_List] :=
  Module[
    {initialState, finalState},

    initialState =
      <|
        "S" -> {},
        "Uncovered" -> X
      |>;

    finalState =
      NestWhile[
        Function[{state}, greedyStep[H, state]],
        initialState,
        Function[{state}, Not[greedyFinishedQ[state]]]
      ];

    finalState["S"] (*restituisce il Representative Set S*)
  ];
  
  (*NestWhile[
    funzione,
    valoreIniziale,
    condizione
]*)


(* --------------------------------------------------------- *)
(*  Algorithm 3: Tabella delle Partizioni                             *)
(* --------------------------------------------------------- *)

ClearAll[
  TabellaPartizioniIniziale,
  removeCoveredFromOtherGroups,
  TabellaPartizioniFinale
];

TabellaPartizioniIniziale[H_Association, S_List] :=
  Association[
    Map[
      Function[{representative}, representative -> H[representative]],
      S
    ]
  ];

removeCoveredFromOtherGroups[partition_Association, h_Integer] := (*prende il gruppo di un rappresentante h e lo toglie da tutti gli altri gruppi.*)
  Module[
    {coveredGroup, updateOneGroup}, 

    coveredGroup = partition[h];  (*per esempio se h=10 \[Rule] coveredGroup = partition[10] = {1,2,5}*)

    updateOneGroup = 
      Function[
        {key},
        If[    (*es. se h=10 e key=20*)
          key == h, (*20 == 10 \[EGrave] falso*)
          key -> partition[key],
          key -> Complement[partition[key], coveredGroup] (*20 -> Complement[partition[20], coveredGroup]*)
        ]                                                 (* se partition[20] = {2,4}  e  coveredGroup = {1,2,5} *)
      ];                                                  (* Complement[{2,4}, {1,2,5}] d\[AGrave] {4} *)
                                                          (* quindi 20 -> {4} *)
    Association[
      Map[
        updateOneGroup,
        Keys[partition]                                   (*Keys[partition] d\[AGrave] tutte le chiavi: {10,20,30}*)
      ]
    ]   (*Association prende una lista di regole (->) e la trasforma in una Association cio\[EGrave] la nuova Partition Table aggiornata.*)
  ];
	(*Map applica updateOneGroup a tutte:
	updateOneGroup[10]
	updateOneGroup[20]
	updateOneGroup[30]
	e ottiene:
	{
	  10 -> {1,2,5},
	  20 -> {4},
	  30 -> {3}
	}*)
TabellaPartizioniFinale[H_Association, S_List] :=
  Fold[
    Function[
      {PartizioneAttuale, representative},
      removeCoveredFromOtherGroups[PartizioneAttuale, representative]
    ],
    TabellaPartizioniIniziale[H, S], (*10 -> {1,2,5},
	                               20 -> {2,4},
	                               30 -> {1,3}*)
    S  (*{10,20,30}*)
  ];
	(* dato lo stato corrente della Partition Table e un rappresentante, costruisci la nuova tabella delle partizioni.
	i rappresentanti vengono considerati uno alla volta, e ogni volta le differenze gi\[AGrave] assegnate vengono eliminate dagli altri gruppi*)


(* --------------------------------------------------------- *)
(*  Algorithm 3 completo                                     *)
(* --------------------------------------------------------- *)

ClearAll[algorithm3];

algorithm3[f_, numeroDiBit_Integer] :=
  Module[
    {size, X, H, S, partition},

    size = 2^numeroDiBit;
    X = AllDiffInput[numeroDiBit];
    Print["Calcolo H su ", size, " differenze..."]; (*es: nBits=8 \[Rule] size = 256 \[Rule] Calcolo H su 256 differenze...*)
    H = calculateH[f, size];
	(*H[y] = lista delle \[Alpha] che producono y*)
    Print["Scelta greedy dei rappresentanti..."];
    S = greedyRappresentanti[H, X];

    Print["Costruzione della Tabella delle Partizioni..."];
    partition = TabellaPartizioniFinale[H, S];

    <|
      "Rappresentanti" -> S,
      "TabellaPartizioni" -> partition,
      "RawH" -> H
    |>
  ];


(* --------------------------------------------------------- *)
(*  Demo                                                     *)
(* --------------------------------------------------------- *)

ClearAll[toySBox, convertiHexLista, partitionRows, runToyDemo];

toySBox[x_Integer] :=
  stratoS[x, 2];

convertiHexLista[Lista_List, numeroCifreEsadecimali_Integer] :=
  Map[
    Function[{valore}, convertiHexElemento[valore, numeroCifreEsadecimali]],
    Lista
  ];

partitionRows[partition_Association, S_List, digits_Integer] :=
  Map[
    Function[
      {representative},
      {
        convertiHexElemento[representative, digits],
        convertiHexLista[partition[representative], digits]
      }
    ],
    S
  ]; (*trasforma la Partition Table (un'Association) in una lista di righe che pu\[OGrave] essere passata a Grid per essere stampata come tabella.*)

runToyDemo[] :=
  Module[
    {result, S, partition, rows},

    result = algorithm3[toySBox, 8];
    S = result["Rappresentanti"];  (*S diventa la lista dei rappresentanti trovati da algorithm3*)
    partition = result["TabellaPartizioni"];
    rows = partitionRows[partition, S, 2];
(*Se ad esempio result fosse:
<|
  "Rappresentanti" -> {3, 7, 12},
  "TabellaPartizioni" -> <|
    3 -> {1, 2},
    7 -> {4, 5},
    12 -> {6}
  |>,
  "RawH" -> <| ... |>
|>
allora dopo questa riga:

partition =
<|
  3 -> {1, 2},
  7 -> {4, 5},
  12 -> {6}
|>*)
    Print[""];
    Print["Numero di rappresentanti trovati: ", Length[S]];
    Print["Representative Set:"];
    Print[convertiHexLista[S, 2]];

    Print[""];
    Print["Tabella delle Partizioni:"];

    Grid[
      Prepend[
        rows,
        {"rappresentante", "elementi del gruppo"}
      ],
      Frame -> All
    ]
  ];


ClearAll[runGiftSmallExperiment];

runGiftSmallExperiment[nBits_Integer] :=
  Module[
    {f, result, S},

    f = superboxRidotta[nBits];
    result = algorithm3[f, nBits];
    S = result["Rappresentanti"];

    Print[""];
    Print["Representative Set:"];
    Print[
      convertiHexLista[
        S,
        Ceiling[nBits/4]
      ]
    ];

    result
  ];


(* --------------------------------------------------------- *)
(*  Comandi consigliati                                      *)
(* --------------------------------------------------------- *)

(*
SetDirectory[NotebookDirectory[]]
  Get["progA4.wl"]

  runToyDemo[]

  runGiftSmallExperiment[8]
*)
