(* ::Package:: *)

(* These first two definitions are prototypes for functions that will be overwritten 
when "InitializeSeff[]" is called from the linked C code for the effective source *)

PsiPmWardell[m_, r_, \[Theta]_] :=
  None

SeffmWardell[m_, r_, \[Theta]_] :=
  None


Delta[a_, rSubrPlus_] := Module[{r, rPlus, rMinus},
  rPlus = 1 + Sqrt[1 - a^2]; rMinus = 1 - Sqrt[1 - a^2]; r = rSubrPlus
     + rPlus; rSubrPlus (r - rMinus)
]


\[CapitalDelta]\[Phi][a_, rSubrPlus_] := Module[{rPlus, rMinus, r},
  rPlus = 1 + Sqrt[1 - a^2]; rMinus = 1 - Sqrt[1 - a^2]; r = rSubrPlus
     + rPlus; a / (rPlus - rMinus) Log[rSubrPlus / (r - rMinus)]
]


d\[CapitalDelta]\[Phi]dr[a_, rSubrPlus_] := Module[{rPlus, rMinus, r},
  rPlus = 1 + Sqrt[1 - a^2]; rMinus = 1 - Sqrt[1 - a^2]; r = rSubrPlus
     + rPlus; a / (rSubrPlus (r - rMinus))
]


PsiPm[m_, a_, r_, \[Theta]_] :=
  Module[{Wardell, BL},
    Wardell = PsiPmWardell[m, N[r], N[\[Theta]]];
    BL = r (Wardell[[1 ;; 10]] + I Wardell[[11 ;; 20]]) Exp[-I m \[CapitalDelta]\[Phi][a,
       r - 1 - Sqrt[1 - a^2]]] / (2 \[Pi]);
    Return[{r^2 / Delta[a, r - 1 - Sqrt[1 - a^2]], 1, 1, r^2 / Delta[
      a, r - 1 - Sqrt[1 - a^2]], 1, 1, 1, 1, 1, r^2 / Delta[a, r - 1 - Sqrt[
      1 - a^2]]} * ({{1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {(-a^2 - r^2) / (a^2 +
       (-2 + r) * r), (a^2 - 2 * r + r^2) / (a^2 + (-2 + r) * r), 0, -(a / 
      (a^2 + (-2 + r) * r)), 0, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0, 0, 0, 0,
       0}, {0, 0, 0, 1, 0, 0, 0, 0, 0, 0}, {(a^4 + 2 * a^2 * r^2 + r^4) / (
      a^2 + (-2 + r) * r) ^ 2, (-2 * a^4 + 4 * a^2 * r - 4 * a^2 * r^2 + 4 
      * r^3 - 2 * r^4) / (a^2 + (-2 + r) * r) ^ 2, 0, (2 * a^3 + 2 * a * r^
      2) / (a^2 + (-2 + r) * r) ^ 2, (a^4 - 4 * a^2 * r + 4 * r^2 + 2 * a^2
       * r^2 - 4 * r^3 + r^4) / (a^2 + (-2 + r) * r) ^ 2, 0, (-2 * a^3 + 4 
      * a * r - 2 * a * r^2) / (a^2 + (-2 + r) * r) ^ 2, 0, 0, a^2 / (a^2 +
       (-2 + r) * r) ^ 2}, {0, 0, (-a^2 - r^2) / (a^2 + (-2 + r) * r), 0, 0,
       1, 0, 0, -(a / (a^2 + (-2 + r) * r)), 0}, {0, 0, 0, (-a^2 - r^2) / (
      a^2 + (-2 + r) * r), 0, 0, 1, 0, 0, -(a / (a^2 + (-2 + r) * r))}, {0,
       0, 0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 1, 0}, {0, 0, 0,
       0, 0, 0, 0, 0, 0, 1}} . BL)];
  ]


Seffm[m_, a_, r_, \[Theta]_] :=
  Module[{Wardell, BL},
    Wardell = SeffmWardell[m, N[r], N[\[Theta]]];
    BL = (Wardell[[1 ;; 10]] + I Wardell[[11 ;; 20]]) Exp[-I m \[CapitalDelta]\[Phi][a, 
      r - 1 - Sqrt[1 - a^2]]] / (2 \[Pi]);
    Return[{{-((r^3 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2), 0,
       0, 0, 0, 0, 0, 0, 0, 0}, {(r^3 + a^2 * r * Cos[\[Theta]] ^ 2) / (a^2 + r^2),
       -((r * (a^2 + (-2 + r) * r) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2
      ) ^ 2), 0, (a * r * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2, 0, 0,
       0, 0, 0, 0}, {0, 0, -((r * (a^2 + (-2 + r) * r) * (r^2 + a^2 * Cos[\[Theta]
      ] ^ 2)) / (a^2 + r^2) ^ 2), 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, -((r^3 * 
      (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2), 0, 0, 0, 0, 0, 0}, {-((
      r * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + (-2 + r) * r)), (2 * r * (r^2 
      + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2), 0, (-2 * a * r * (r^2 + a^2 * Cos[
      \[Theta]] ^ 2)) / ((a^2 + (-2 + r) * r) * (a^2 + r^2)), -((r * (a^2 + (-2 + 
      r) * r) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2), 0, (2 * a * r
       * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2, 0, 0, -((a^2 * r * (r
      ^2 + a^2 * Cos[\[Theta]] ^ 2)) / ((a^2 + (-2 + r) * r) * (a^2 + r^2) ^ 2))},
       {0, 0, (r^3 + a^2 * r * Cos[\[Theta]] ^ 2) / (a^2 + r^2), 0, 0, -((r * (a^2
       + (-2 + r) * r) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2), 0, 0,
       (a * r * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2, 0}, {0, 0, 0, 
      (r^3 + a^2 * r * Cos[\[Theta]] ^ 2) / (a^2 + r^2), 0, 0, -((r * (a^2 + (-2 +
       r) * r) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2), 0, 0, (a * r
       * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2}, {0, 0, 0, 0, 0, 0, 0,
       -((r * (a^2 + (-2 + r) * r) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2
      ) ^ 2), 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, -((r * (a^2 + (-2 + r) * r) *
       (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2), 0}, {0, 0, 0, 0, 0, 0,
       0, 0, 0, -((r^3 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)) / (a^2 + r^2) ^ 2)}} . BL
      ];
  ]


Sigma[a_, rSubrPlus_, \[Theta]_] := Module[{r, rPlus, rMinus},
  rPlus = 1 + Sqrt[1 - a^2]; rMinus = 1 - Sqrt[1 - a^2]; r = rSubrPlus
     + rPlus; Sqrt[(r^2 + a^2) ^ 2 - a^2 Delta[a, rSubrPlus] Sin[\[Theta]] ^ 2]
]


SigmaSq[a_, rSubrPlus_, \[Theta]_] := Module[{r, rPlus, rMinus},
  rPlus = 1 + Sqrt[1 - a^2]; rMinus = 1 - Sqrt[1 - a^2]; r = rSubrPlus
     + rPlus; (r^2 + a^2) ^ 2 - a^2 Delta[a, rSubrPlus] Sin[\[Theta]] ^ 2
]


drStardr[a_, rSubrPlus_] := Module[{rPlus, rMinus, r},
  rPlus = 1 + Sqrt[1 - a^2]; r = rSubrPlus + rPlus; (r^2 + a^2) / Delta[
    a, rSubrPlus]
]


getrStarFromrSubrPlus[a_, rSubrPlus_] :=
  Module[{r, rPlus, rMinus},
    rPlus = 1 + Sqrt[1 - a^2]; rMinus = 1 - Sqrt[1 - a^2]; r = rSubrPlus
       + rPlus; r + 2 / (rPlus - rMinus) (rPlus Log[rSubrPlus / 2] - rMinus
       Log[(r - rMinus) / 2])
  ]


getrSubrPlusFromrStar[a_, rStar_] :=
  Module[{rSubrPlus, guess, rPlus},
    rPlus = 1 + Sqrt[1 - a^2];
    guess =
      If[rStar < -2,
        2 (1 - a^2) ^ (1 / rPlus - 1 / 2) Exp[(a^2 - rPlus + (rPlus -
           1) * rStar) / rPlus]
        ,
        If[rStar < 1000,
          2 * ProductLog[Exp[(rStar - rPlus) / 2]]
          ,
          rStar
        ]
      ];
    Re[
      rSubrPlus /.
        Quiet[
          Check[
            If[rStar < 10,
              FindRoot[getrStarFromrSubrPlus[a, rSubrPlus] - rStar, {
                rSubrPlus, guess}, AccuracyGoal -> 13, PrecisionGoal -> 13]
              ,
              FindRoot[getrStarFromrSubrPlus[a, rSubrPlus] / rStar - 
                1, {rSubrPlus, guess}, AccuracyGoal -> 13, PrecisionGoal -> 13]
            ]
            ,
            NSolve[getrStarFromrSubrPlus[a, rSubrPlus] / rStar - 1 ==
               0, rSubrPlus, Reals][[1]]
          ]
        ]
    ]
  ]


getrStarParams[a_, r0_, wtDiam_, rStarHguess_, rStarIguess_] :=
  Module[{rPlus, rStar0, rStarL, rStarR, rStarH, rStarI},
    rPlus = 1 + Sqrt[1 - a^2];
    rStar0 = getrStarFromrSubrPlus[a, r0 - rPlus];
    rStarL = rStar0 - 0.5 wtDiam;
    rStarR = rStar0 + 0.5 wtDiam;
    rStarI = rStarR + wtDiam Ceiling[(rStarIguess - rStarR) / wtDiam]
      ;
    rStarH = rStarL - wtDiam Ceiling[(rStarL - rStarHguess) / wtDiam]
      ;
    {rStarH, rStarL, rStar0, rStarR, rStarI}
  ]


getrStarList[\[CapitalDelta]rStar_, rStarH_, rStarL_, rStar0_, rStarR_, rStarI_] :=
  Module[{wtDiam, wtNum, new\[CapitalDelta]rStar, totalNum},
    wtDiam = rStarR - rStarL;
    wtNum = 2 Round[(1 + wtDiam / \[CapitalDelta]rStar) / 2];
    new\[CapitalDelta]rStar = wtDiam / (wtNum - 1);
    totalNum = Round[(rStarI - rStarH) / new\[CapitalDelta]rStar] + 1;
    Return[Table[rStarH + (i - 1) new\[CapitalDelta]rStar, {i, 1, totalNum}]]
  ]


getrSubrPlusList[a_, rStarList_] := Table[getrSubrPlusFromrStar[a, rStar
  ], {rStar, rStarList}]


getIsourceBounds[rStarList_, rStarH_, rStarL_, rStar0_, rStarR_, rStarI_
  ] := Module[{\[CapitalDelta]rStar},
  \[CapitalDelta]rStar = (rStarList[[-1]] - rStarList[[1]]) / (Length[rStarList] - 
    1); Return[{1 + Round[(rStarL - rStarH) / \[CapitalDelta]rStar], 1 + Round[(rStarR 
    - rStarH) / \[CapitalDelta]rStar]}]
]


getThetaListOld[\[CapitalDelta]\[Theta]_] := Module[{},
Table[\[Theta], {\[Theta], 0, \[Pi], \[CapitalDelta]\[Theta]}]]


getThetaList[\[CapitalDelta]\[Theta]_, thetaSourceSize_] := Module[{numSourceInPi, newThetaSourceSize,
   sourceNum, nTheta, new\[CapitalDelta]\[Theta], totalNum},
  numSourceInPi = If[OddQ[Round[\[Pi] / thetaSourceSize]] == True,
    Round[\[Pi] / thetaSourceSize]
    ,
    2 (Round[\[Pi] / (2 thetaSourceSize)] + 1 / 2)
  ];
  newThetaSourceSize = N[\[Pi] / numSourceInPi];
  sourceNum = 2 Round[(1 + newThetaSourceSize / \[CapitalDelta]\[Theta]) / 2];
  new\[CapitalDelta]\[Theta] = newThetaSourceSize / (sourceNum - 1);
  totalNum = Round[\[Pi] / new\[CapitalDelta]\[Theta]] + 1;
  Return[Table[(j - 1) new\[CapitalDelta]\[Theta], {j, 1, totalNum}]]
]


getJsourceBounds[\[CapitalDelta]\[Theta]_, thetaSourceSize_] := Module[{numSourceInPi, newThetaSourceSize,
   sourceNum, nTheta, new\[CapitalDelta]\[Theta], totalNum},
  numSourceInPi = If[OddQ[Round[\[Pi] / thetaSourceSize]] == True,
    Round[\[Pi] / thetaSourceSize]
    ,
    2 (Round[\[Pi] / (2 thetaSourceSize)] + 1 / 2)
  ];
  newThetaSourceSize = N[\[Pi] / numSourceInPi];
  sourceNum = 2 Round[(1 + newThetaSourceSize / \[CapitalDelta]\[Theta]) / 2];
  new\[CapitalDelta]\[Theta] = newThetaSourceSize / (sourceNum - 1);
  totalNum = Round[\[Pi] / new\[CapitalDelta]\[Theta]] + 1;
  Return[{1 + Round[(\[Pi] / 2 - newThetaSourceSize / 2) / new\[CapitalDelta]\[Theta]], 1 + Round[
    (\[Pi] / 2 + newThetaSourceSize / 2) / new\[CapitalDelta]\[Theta]]}]
]


Afunc[m_, a_, \[Omega]_, r_List, \[Theta]_] :=
  {{2 * ((-3 * a^4 + I * a^3 * m * r + a^2 * (4 - 3 * r) * r + 2 * r^
    3 + I * a * m * r^3) / (r * (a^2 + r^2) ^ 2) + ((-I) * r + a * Cos[\[Theta]]
    ) ^ (-2) + (I * r + a * Cos[\[Theta]]) ^ (-2)), ConstantArray[0, {Length[r]}
    ], ConstantArray[0, {Length[r]}], (4 * a * (-r^2 + a^2 * Cos[\[Theta]] ^ 2))
     / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}]}, {(-2 * a^2 * (a^2 + (-2 + r) * r) * Sin[\[Theta]] ^ 2) / (r * (a^2 + 
    r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ((-I) * r + a * Cos[\[Theta]]) ^ (-2) + (I
     * r + a * Cos[\[Theta]]) ^ (-2) + (2 * r^2 * (-2 * a^4 + I * a^3 * m * r + 
    a^2 * (3 - 2 * r) * r + r^3 + I * a * m * r^3) + 2 * a^2 * (-a^4 + a^
    2 * (1 + I * a * m) * r + (-1 + I * a * m) * r^3 + r^4) * Cos[\[Theta]] ^ 2)
     / ((a^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]] ^ 2)), (a^2 * Sin[2 * \[Theta]
    ]) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-2 * a * (a^2 + (-2 +
     r) * r)) / (r * (a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], (2 * a * (-r^2 + a^2 
    * Cos[\[Theta]] ^ 2)) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}]}, {-((a^2 * (a^2 + (-2 + r) * r) * Sin[2 * \[Theta]]) / (r^4 + a^2 * r^
    2 * Cos[\[Theta]] ^ 2)), (-2 * a^2 * (a^2 + (-2 + r) * r) * Cos[\[Theta]] * Sin[\[Theta]])
     / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-2 * a * (a^3 - I * a^2
     * m * r + a * (-2 + r) * r - I * m * r^3)) / (r * (a^2 + r^2) ^ 2) +
     (a^2 + (-2 + r) * r) / ((a^2 + r^2) * (-r + I * a * Cos[\[Theta]])) - (a^2 
    + (-2 + r) * r) / ((a^2 + r^2) * (r + I * a * Cos[\[Theta]])) + ((-I) * r + 
    a * Cos[\[Theta]]) ^ (-2) + (I * r + a * Cos[\[Theta]]) ^ (-2), -((a^3 * (a^2 + (-2
     + r) * r) * Sin[2 * \[Theta]]) / (r^2 * (a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^
     2))), ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r]}], 
    ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r]}], (2 * a 
    * (-r^2 + a^2 * Cos[\[Theta]] ^ 2)) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2
    ) ^ 2), ConstantArray[0, {Length[r]}]}, {(2 * a * (r^2 * (a^2 + 3 * r
    ^2) + a^2 * (-a + r) * (a + r) * Cos[\[Theta]] ^ 2) * Sin[\[Theta]] ^ 2) / ((a^2 + 
    r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[0, {Length[r]}], 
    ConstantArray[0, {Length[r]}], (-6 * a^4 + (2 * I) * a^3 * m * r + 2 
    * a^2 * (5 - 4 * r) * r + (2 * I) * a * m * r^3 - 2 * (-3 + r) * r^3)
     / (r * (a^2 + r^2) ^ 2), ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], (2 * a * (-r^2 + a^2 * Cos[\[Theta]] ^ 
    2)) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2)}, {ConstantArray[0,
     {Length[r]}], (-4 * a^2 * r * Sin[\[Theta]] ^ 2) / ((a^2 + r^2) * (r^2 + a^
    2 * Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}], (2 * r^2 * (-3 * a^4 + I * a^3 * m * r + a^2 * (4 - 3 * r) * r 
    + 2 * r^3 + I * a * m * r^3) + 2 * a^2 * (-a^4 + I * a^3 * m * r + a^
    2 * r^2 + I * a * m * r^3 + 2 * (-1 + r) * r^3) * Cos[\[Theta]] ^ 2) / ((a^2
     + r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]] ^ 2)), (4 * a^2 * Cos[\[Theta]] * Sin[
    \[Theta]]) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-4 * a * r) / ((a^2 
    + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}]}, {ConstantArray[0, {Length[
    r]}], (-2 * a^2 * Cos[\[Theta]] * Sin[\[Theta]]) / (r^2 + a^2 * Cos[\[Theta]] ^ 2), (-2 * 
    a^2 * r * Sin[\[Theta]] ^ 2) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[
    0, {Length[r]}], (-2 * a^2 * (a^2 + (-2 + r) * r) * Cos[\[Theta]] * Sin[\[Theta]]) 
    / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (2 * (a^2 + I * a^3 * m -
     a^4 / r + I * a * m * r^2 + (-1 + r) * r^2 - (4 * r * ((-I) * a + r)
     * (I * a + r) * (a^2 + (-2 + r) * r)) / (a^2 + 2 * r^2 + a^2 * Cos[2
     * \[Theta]]))) / (a^2 + r^2) ^ 2, (-2 * a^3 * Cos[\[Theta]] * Sin[\[Theta]]) / ((a^2 + r^
    2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (a^2 * Sin[2 * \[Theta]]) / ((a^2 + r^2) * (
    r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-2 * a * r) / ((a^2 + r^2) * (r^2 + a^2 * 
    Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[r]}]}, {ConstantArray[0, {Length[
    r]}], (2 * a * (r^2 * (a^2 + 3 * r^2) + a^2 * (-a + r) * (a + r) * Cos[
    \[Theta]] ^ 2) * Sin[\[Theta]] ^ 2) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2),
     ConstantArray[0, {Length[r]}], (-2 * a^2 * (a^2 + (-2 + r) * r) * Sin[
    \[Theta]] ^ 2) / (r * (a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], (-2 * a * (a^3 - I * 
    a^2 * m * r + a * (-2 + r) * r - I * m * r^3)) / (r * (a^2 + r^2) ^ 2
    ) + (r - I * a * Cos[\[Theta]]) ^ (-2) + (a^2 + (-2 + r) * r) / ((a^2 + r^2)
     * (-r + I * a * Cos[\[Theta]])) + (r + I * a * Cos[\[Theta]]) ^ (-2) - (a^2 + (-2 
    + r) * r) / ((a^2 + r^2) * (r + I * a * Cos[\[Theta]])), ConstantArray[0, {Length[
    r]}], (a^2 * Sin[2 * \[Theta]]) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), 
    (-2 * a * (a^2 + (-2 + r) * r)) / (r * (a^2 + r^2) * (r^2 + a^2 * Cos[
    \[Theta]] ^ 2))}, {ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r
    ]}], (-4 * a^2 * Cos[\[Theta]] * Sin[\[Theta]]) / (r^2 + a^2 * Cos[\[Theta]] ^ 2), ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], (-4 * a^2 * (a^2 + (-
    2 + r) * r) * Cos[\[Theta]] * Sin[\[Theta]]) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^
     2)), ConstantArray[0, {Length[r]}], (2 * r^2 * (-3 * a^4 + I * a^3 *
     m * r + a^2 * (6 - 5 * r) * r + I * a * m * r^3 - 2 * (-2 + r) * r^3
    ) - 2 * a^3 * (a^3 - I * a^2 * m * r + a * (-2 + r) * r - I * m * r^3
    ) * Cos[\[Theta]] ^ 2) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]] ^ 2)), (
    -4 * a^3 * Cos[\[Theta]] * Sin[\[Theta]]) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)
    ), ConstantArray[0, {Length[r]}]}, {ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], (2 * a * (r^2 * (a^2 + 3 * r^2) + a^2 * (-a + r) * (
    a + r) * Cos[\[Theta]] ^ 2) * Sin[\[Theta]] ^ 2) / ((a^2 + r^2) * (r^2 + a^2 * Cos[
    \[Theta]] ^ 2) ^ 2), -((a^2 * (a^2 + (-2 + r) * r) * Sin[2 * \[Theta]]) / (r^4 + a^
    2 * r^2 * Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], (-2 * a^2 * (a^2 + (-2 + r) * r) * Cos[\[Theta]] * Sin[\[Theta]]) 
    / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[
    r]}], (r - I * a * Cos[\[Theta]]) ^ (-2) + (r + I * a * Cos[\[Theta]]) ^ (-2) + (2 
    * r^2 * (-2 * a^4 + I * a^3 * m * r + a^2 * (5 - 4 * r) * r + I * a *
     m * r^3 + (3 - 2 * r) * r^3) - 2 * a^2 * (a^4 - I * a^3 * m * r - I 
    * a * m * r^3 + (-1 + r) * r^3 + a^2 * r * (-3 + 2 * r)) * Cos[\[Theta]] ^ 2
    ) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]] ^ 2)), -((a^3 * (a^2 +
     (-2 + r) * r) * Sin[2 * \[Theta]]) / (r^2 * (a^2 + r^2) * (r^2 + a^2 * Cos[
    \[Theta]] ^ 2)))}, {ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], (4 * a * (r^2 * (a^2 + 3 * r^2) 
    + a^2 * (-a + r) * (a + r) * Cos[\[Theta]] ^ 2) * Sin[\[Theta]] ^ 2) / ((a^2 + r^2)
     * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], (-6 * a^4 + (2 * I) * a^3 * m * 
    r + 2 * a^2 * (6 - 5 * r) * r + (2 * I) * a * m * r^3 - 4 * (-2 + r) 
    * r^3) / (r * (a^2 + r^2) ^ 2) + 2 * ((r - I * a * Cos[\[Theta]]) ^ (-2) + (
    r + I * a * Cos[\[Theta]]) ^ (-2))}};


Bfunc[m_, a_, \[Omega]_, r_List, \[Theta]_] :=
  {{-(((a^2 + (-2 + r) * r) * (8 * a^2 * r + r^4 + 2 * a^2 * (-4 + r)
     * r * Cos[\[Theta]] ^ 2 + a^4 * Cos[\[Theta]] ^ 4) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * 
    (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2)), ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], (-8 * a * r * (a^2 + (-2 + r) * r) * Cot[\[Theta]]) / ((a^2
     + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}]}, {(-2 * (a^3 + a * (-2 + r) * r) ^ 2 * Sin[2 * \[Theta]]) / (r^2 * (a^
    2 + r^2) ^ 2 * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]])), ((a^2 + (-2 + r) 
    * r) * (-(r * (r^3 + 2 * a^2 * (2 + r))) - 2 * a^2 * (a^2 - 2 * r) * 
    Cos[\[Theta]] ^ 2 + a^4 * Cos[\[Theta]] ^ 4) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + 
    a^2 * Cos[\[Theta]] ^ 2) ^ 2), (-2 * r * (a^2 + (-2 + r) * r)) / ((a^2 + r^2
    ) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-2 * a * (a^2 + (-2 + r) * r) ^ 2
     * Cot[\[Theta]]) / (r^2 * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], (-4 * a * r * (a^2 + 
    (-2 + r) * r) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)
     ^ 2), ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r]}], 
    ConstantArray[0, {Length[r]}]}, {(2 * (a^2 + (-2 + r) * r) ^ 2) / (r 
    * (a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (2 * r * (a^2 + (-2 + r) *
     r) ^ 2) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ((a^2 + (-2 
    + r) * r) * (-(r * (r^3 + 2 * a^2 * (2 + r))) - 2 * a^2 * (a^2 - 2 * 
    r) * Cos[\[Theta]] ^ 2 + a^4 * Cos[\[Theta]] ^ 4) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r
    ^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (2 * a * (a^2 + (-2 + r) * r) ^ 2) / ((a
    ^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], (-4 * a * r * (a^2 + (-2 + r) * r) * Cot[\[Theta]]) / ((a^2
     + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[0, {Length[
    r]}]}, {(4 * a^3 * r * (a^2 + (-2 + r) * r) * Cos[\[Theta]] * Sin[\[Theta]] ^ 3) / 
    ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], ((a^2 + (-2 + r) * r) * Cot[\[Theta]]) 
    / (a^2 + r^2) ^ 2, ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], (-4 * a * r * (a^2 + (-2 + r) * r) * Cot[\[Theta]]) / ((a^2
     + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2)}, {ConstantArray[0, {Length[
    r]}], (-4 * a^2 * (a^2 + (-2 + r) * r) * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^
     2 * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]])), ConstantArray[0, {Length[r]
    }], ConstantArray[0, {Length[r]}], ((a^2 + (-2 + r) * r) * (-4 * a^2 
    - r^2 + 3 * a^2 * Cos[\[Theta]] ^ 2) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a
    ^2 * Cos[\[Theta]] ^ 2)), (-4 * r * (a^2 + (-2 + r) * r)) / ((a^2 + r^2) ^ 2
     * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-4 * a * (a^2 + (-2 + r) * r) * Cot[\[Theta]]
    ) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r]}]}, 
    {ConstantArray[0, {Length[r]}], (2 * r * (a^2 + (-2 + r) * r)) / ((a^
    2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-2 * a^2 * (a^2 + (-2 + r) * r
    ) * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]
    ])), ConstantArray[0, {Length[r]}], (2 * r * (a^2 + (-2 + r) * r) ^ 2
    ) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ((a^2 + (-2 + r) * 
    r) * (-4 * a^2 - r^2 + 3 * a^2 * Cos[\[Theta]] ^ 2) * Cot[\[Theta]]) / ((a^2 + r^2)
     ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (2 * a * r * (a^2 + (-2 + r) * r)) 
    / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-2 * r * (a^2 + (-2 
    + r) * r)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-2 * a * 
    (a^2 + (-2 + r) * r) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[
    \[Theta]] ^ 2)), ConstantArray[0, {Length[r]}]}, {ConstantArray[0, {Length[r
    ]}], (4 * a^3 * r * (a^2 + (-2 + r) * r) * Cos[\[Theta]] * Sin[\[Theta]] ^ 3) / ((a
    ^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[0, {Length[
    r]}], (-2 * (a^3 + a * (-2 + r) * r) ^ 2 * Sin[2 * \[Theta]]) / (r^2 * (a^2 
    + r^2) ^ 2 * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]])), ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], ((a^2 + (-2 + r) * r) * (-2 * a^
    2 * (-2 + r) * r + r^4 - 2 * a^2 * (a^2 - 2 * (-1 + r) * r) * Cos[\[Theta]] 
    ^ 2 + 3 * a^4 * Cos[\[Theta]] ^ 4) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a^2
     * Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[0, {Length[r]}], (-2 * r * (a^2 + 
    (-2 + r) * r)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (-2 * 
    a * (a^2 + (-2 + r) * r) ^ 2 * Cot[\[Theta]]) / (r^2 * (a^2 + r^2) ^ 2 * (r^
    2 + a^2 * Cos[\[Theta]] ^ 2))}, {ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], (4 * r * (a^2 + (-2 + r) * r)) / ((a^2 + r^2) * (r^2
     + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[r]}], ConstantArray[0,
     {Length[r]}], (4 * r * (a^2 + (-2 + r) * r) ^ 2) / ((a^2 + r^2) ^ 2 
    * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[r]}], ((a^2 + (
    -2 + r) * r) * (-4 * a^2 - r^2 + 3 * a^2 * Cos[\[Theta]] ^ 2) * Cot[\[Theta]]) / ((
    a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (4 * a * r * (a^2 + (-2 +
     r) * r)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[
    0, {Length[r]}]}, {ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}], (4 * a^3 * r * (a^2 + (-2 + r) * r) * Cos[\[Theta]] * Sin[\[Theta]] ^ 3) / ((
    a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (2 * (a^2 + (-2 + r) 
    * r) ^ 2) / (r * (a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[
    0, {Length[r]}], ConstantArray[0, {Length[r]}], (2 * r * (a^2 + (-2 +
     r) * r) ^ 2) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[
    0, {Length[r]}], ((a^2 + (-2 + r) * r) * (-2 * a^2 * (-2 + r) * r + r
    ^4 - 2 * a^2 * (a^2 - 2 * (-1 + r) * r) * Cos[\[Theta]] ^ 2 + 3 * a^4 * Cos[
    \[Theta]] ^ 4) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2),
     (2 * a * (a^2 + (-2 + r) * r) ^ 2) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 *
     r * Cos[\[Theta]] ^ 2))}, {ConstantArray[0, {Length[r]}], ConstantArray[0, 
    {Length[r]}], ConstantArray[0, {Length[r]}], (8 * a^3 * r * (a^2 + (-
    2 + r) * r) * Cos[\[Theta]] * Sin[\[Theta]] ^ 3) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * 
    Cos[\[Theta]] ^ 2) ^ 2), ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[
    r]}], ConstantArray[0, {Length[r]}], ConstantArray[0, {Length[r]}], ConstantArray[
    0, {Length[r]}], ((a^2 + (-2 + r) * r) * (8 * a^2 * r + 3 * r^4 + 2 *
     a^2 * r * (-4 + 3 * r) * Cos[\[Theta]] ^ 2 + 3 * a^4 * Cos[\[Theta]] ^ 4) * Cot[\[Theta]]
    ) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2)}};


Cfunc[m_, a_, \[Omega]_, r_List, \[Theta]_] :=
  {{(((-12 * I) * a^3 * m) / r - 4 * a * m * (I * (-4 + r) + 2 * r * 
    \[Omega]) + a^4 * (24 / r^2 + \[Omega]^2) + 2 * (16 - 6 * r + r^4 * \[Omega]^2) + (a^2 * (
    -60 + r * (16 + r * (2 + 3 * r) * \[Omega]^2))) / r + a^2 * (a^2 + (-2 + r) 
    * r) * \[Omega]^2 * Cos[2 * \[Theta]] + (16 * (-48 * r^3 * (a^2 + (-2 + r) * r) * (
    a^2 + r^2) * (a^2 + r + r^2) + 2 * r * (a^2 + (-2 + r) * r) * (9 * a^
    4 + 2 * a^2 * r * (5 + 11 * r) + r^3 * (22 + 13 * r)) * (a^2 + 2 * r^
    2 + a^2 * Cos[2 * \[Theta]]) + r * (9 * a^4 - 20 * a^2 * r + 20 * r^2 + 10 *
     a^2 * r^2 - 20 * r^3 + r^4 - (4 * I) * r * (a^2 + r^2) ^ 2 * \[Omega]) * (a
    ^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 2 + ((a^2 + r^2) * (-3 * a^2 - (-4
     + r) * r + I * r * (a^2 + r^2) * \[Omega]) * (a^2 + 2 * r^2 + a^2 * Cos[2 *
     \[Theta]]) ^ 3) / r)) / (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 4 - 2 * m^2 * 
    (a^2 + (-2 + r) * r) * Csc[\[Theta]] ^ 2) / (2 * (a^2 + r^2) ^ 2), (4 * r^2 
    * (r^3 * (-3 * a^4 + I * a * m * r^3 + a^2 * r * (3 + r * (-5 - I * r
     * \[Omega])) + r^3 * (3 + r * (-2 - I * r * \[Omega]))) + a^2 * r * (9 * a^4 + I *
     a * m * r^3 + r^3 * (-10 + r * (7 - I * r * \[Omega])) + a^2 * r * (-10 + r
     * (16 - I * r * \[Omega]))) * Cos[\[Theta]] ^ 2 + I * a^4 * (-(a * m * r^2) + a^2 
    * (I + r * (3 * I + r * \[Omega])) + r^2 * (I + r * (3 * I + r * \[Omega]))) * Cos[
    \[Theta]] ^ 4 + I * a^6 * (-(a * m) + (a^2 + r^2) * \[Omega]) * Cos[\[Theta]] ^ 6)) / ((a^
    2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), ((4 * I) * a * r^2 * ((
    9 * I) * a^3 * r^2 + 2 * m * r^5 + a * r^4 * (9 * I - 2 * r * \[Omega]) + a 
    * ((-3 * I) * a^4 + 4 * a * m * r^3 - 4 * a^2 * r^2 * (3 * I + r * \[Omega])
     + r^4 * (-9 * I + 2 * r * \[Omega])) * Cos[\[Theta]] ^ 2 + a^3 * Cos[\[Theta]] ^ 4 * (2 *
     a * m * r + a^2 * (3 * I - r * \[Omega]) + r^2 * (3 * I + 4 * r * \[Omega]) + a^2 
    * r * \[Omega] * Cos[2 * \[Theta]])) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[
    \[Theta]] ^ 2) ^ 4), (4 * a * (-(r^4 * (3 * a^4 + 2 * a^2 * (-2 + r) * r + r
    ^2 * (-4 + r * (2 + r)))) - I * r^7 * (a^2 + r^2) * \[Omega] + a^2 * r^2 * (
    9 * a^4 + a^2 * r * (-8 + r * (19 - I * r * \[Omega])) + r^2 * (-20 + r * (-
    8 + r * (8 - I * r * \[Omega])))) * Cos[\[Theta]] ^ 2 + a^4 * r^2 * (-6 * a^2 + 2 *
     (5 - 2 * r) * r + I * r * (a^2 + r^2) * \[Omega]) * Cos[\[Theta]] ^ 4 + a^6 * (-3 
    * a^2 - (-4 + r) * r + I * r * (a^2 + r^2) * \[Omega]) * Cos[\[Theta]] ^ 6)) / (r *
     (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (-2 * r^2 * (a^2 + 
    (-2 + r) * r) * (r^3 * (3 * a^2 + r * (-3 + 2 * r)) + a^2 * r * (-9 *
     a^2 + (10 - 7 * r) * r) * Cos[\[Theta]] ^ 2 + a^4 * (1 + 3 * r) * Cos[\[Theta]] ^ 
    4)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (12 * a^2 * r
    ^2 * (a^2 + (-2 + r) * r) * Cos[\[Theta]] * (-3 * r^2 + a^2 * Cos[\[Theta]] ^ 2) * 
    Sin[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (-4 * a *
     r^2 * (r^3 * (3 * a^2 + r * (-3 + 2 * r)) + a^2 * r * (-9 * a^2 + (10
     - 7 * r) * r) * Cos[\[Theta]] ^ 2 + a^4 * (1 + 3 * r) * Cos[\[Theta]] ^ 4)) / ((a^
    2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (2 * r^3 * (r^2 * (3 * 
    a^2 + (-2 + r) * r) + a^2 * (-9 * a^2 - 5 * (-2 + r) * r) * Cos[\[Theta]] ^ 
    2 + 6 * a^4 * Cos[\[Theta]] ^ 4)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^
     2) ^ 4), (12 * a^3 * r^2 * Cos[\[Theta]] * (-3 * r^2 + a^2 * Cos[\[Theta]] ^ 2) * 
    Sin[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (2 * r * 
    (a^2 + (-2 + r) * r) * (r^2 * (-3 * a^2 + (-2 + r) * r) + a^2 * Cos[\[Theta]
    ] ^ 2 * (3 * a^2 + r * (10 + r) - 6 * a^2 * Cos[2 * \[Theta]])) * Csc[\[Theta]] ^ 2
    ) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4)}, {((a^2 + (-2 +
     r) * r) * (2 * (4 * a^8 - (3 * I) * a^7 * m * r + I * a^5 * m * (6 -
     11 * r) * r^2 - (16 * I) * a^3 * m * (-1 + r) * r^4 - (8 * I) * a * 
    m * (-2 + r) * r^6 - 8 * (-2 + r) * r^7 + a^6 * r * (-2 + 12 * r - I 
    * r * \[Omega]) + 4 * a^2 * r^4 * (2 + r * (-3 - 2 * r - (2 * I) * r * \[Omega])) +
     a^4 * r^2 * (-10 + r * (-13 + 13 * r - (4 * I) * r * \[Omega]))) + a^2 * (9
     * a^6 - (8 * I) * a^5 * m * r - (16 * I) * a * m * (-2 + r) * r^4 - 
    (8 * I) * a^3 * m * r^2 * (-2 + 3 * r) - 8 * a^2 * r^3 * (1 + 3 * r) 
    + a^4 * r * (-17 + 16 * r - I * r * \[Omega]) + 8 * r^4 * (-2 + r * (7 - 2 *
     r + (2 * I) * r * \[Omega]))) * Cos[2 * \[Theta]] + 2 * a^4 * r * ((-I) * a^3 * m 
    - I * a * m * (-2 + r) * r + r * (10 + r + r^2 * (-1 + (4 * I) * \[Omega])) 
    + a^2 * (-6 + I * r * (4 * I + \[Omega]))) * Cos[4 * \[Theta]] + a^6 * (-a^2 + r + 
    I * r^2 * \[Omega]) * Cos[6 * \[Theta]])) / (8 * r^2 * (a^2 + r^2) ^ 2 * (r^2 + a^2
     * Cos[\[Theta]] ^ 2) ^ 3), ((4 * a * (a - I * m * r) * (a^2 + (-2 + r) * r)
    ) / r^2 + (4 * I) * (-a^2 + (2 * I) * a * m * r + r^2) * \[Omega] + (a^4 + 2
     * r^4 + a^2 * r * (2 + 3 * r)) * \[Omega]^2 + a^2 * (a^2 + (-2 + r) * r) * 
    \[Omega]^2 * Cos[2 * \[Theta]] + (32 * r * (a^4 - 2 * a^2 * r + 6 * a^2 * r^2 - 10 
    * r^3 + 5 * r^4 - I * r * (a^2 + r^2) ^ 2 * \[Omega])) / (a^2 + 2 * r^2 + a^
    2 * Cos[2 * \[Theta]]) ^ 2 + (8 * (a^2 + (-2 + r) * r) * (a^2 * (-1 + r) - I
     * a * m * r^2 - r^2 * (7 + r)) + (8 * I) * r * (a^2 + r^2) ^ 2 * \[Omega]) 
    / (r * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]])) - 2 * m^2 * (a^2 + (-2 + r
    ) * r) * Csc[\[Theta]] ^ 2) / (2 * (a^2 + r^2) ^ 2), (2 * r * (I * a * m + r
    ) * (a^2 + (-2 + r) * r) * Cot[\[Theta]] - a^2 * (a^2 + r * (-2 + r - (2 * I
    ) * r * \[Omega])) * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]
    ] ^ 2)), ((a^2 + (-2 + r) * r) * (a * (9 * a^6 + (12 * I) * a^5 * m *
     r + (16 * I) * a * m * (-2 + r) * r^4 + (4 * I) * a^3 * m * r^2 * (-
    6 + 7 * r) + a^4 * r * (28 + 27 * r - (6 * I) * r * \[Omega]) + 8 * r^4 * (4
     + r * (-6 + r - (2 * I) * r * \[Omega])) + 8 * a^2 * r^2 * (-10 + r * (-1 +
     4 * r - (2 * I) * r * \[Omega]))) + 4 * a^3 * (3 * a^4 + I * a^3 * m * r + 
    I * a * m * (-2 + r) * r^2 + a^2 * r * (6 + 7 * r - (2 * I) * r * \[Omega]) 
    + 2 * r^2 * (-10 + r * (1 + r - (2 * I) * r * \[Omega]))) * Cos[2 * \[Theta]] + a^5
     * (3 * a^2 + r * (-4 + r - (2 * I) * r * \[Omega])) * Cos[4 * \[Theta]] - (8 * I) 
    * m * r * (a^2 + (-2 + r) * r) * (a^2 + r^2) ^ 2 * Csc[\[Theta]] ^ 2)) / (4 
    * r^2 * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), (2 * (a^2 + 
    (-2 + r) * r) * (r^3 * (a^2 - 3 * r + I * a * m * r + 2 * r^2 - I * r
     * (a^2 + r^2) * \[Omega]) - 5 * a^2 * r * (a^2 + (-2 + r) * r) * Cos[\[Theta]] ^ 2
     + a^4 * (1 - I * a * m - r + I * (a^2 + r^2) * \[Omega]) * Cos[\[Theta]] ^ 4)) / (
    (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), ((I / 2) * a * (a^2 
    + (-2 + r) * r) * (4 * a^2 * m * r + 8 * m * r^3 + a^3 * (2 * I - r *
     \[Omega]) - 4 * a * r^2 * (4 * I + r * \[Omega]) + 4 * a * r * (a * m + r * (4 * I
     + r * \[Omega])) * Cos[2 * \[Theta]] + a^3 * (-2 * I + r * \[Omega]) * Cos[4 * \[Theta]]) * Cot[
    \[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), (a * (-a^2 - 
    (-2 + r) * r + I * r * (a^2 + r^2) * \[Omega]) * (a^2 - 2 * r^2 + a^2 * Cos[
    2 * \[Theta]])) / (r * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (r *
     (a^2 + (-2 + r) * r) * (5 * a^2 - 2 * r^2 + 5 * a^2 * Cos[2 * \[Theta]])) /
     ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), ConstantArray[0, {
    Length[r]}], ((a^2 + (-2 + r) * r) ^ 2 * (5 * a^2 - 2 * r^2 + 5 * a^2
     * Cos[2 * \[Theta]]) * Csc[\[Theta]] ^ 2) / (r * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[
    \[Theta]] ^ 2) ^ 3)}, {(a^2 * (a^2 + (-2 + r) * r) * Cos[\[Theta]] * (-2 * r^3 * (a
    ^2 + (-2 + r) * r) * (-(a^2 * (-10 + r)) + I * a * m * r^2 + r^2 * (10
     + r)) - (4 * I) * r^6 * (a^2 + r^2) * \[Omega] + 4 * a^2 * r * ((a^2 + (-2 
    + r) * r) * ((-I) * a * m * r^2 - (-1 + r) * r^2 + a^2 * (1 + r)) - (
    2 * I) * r^3 * (a^2 + r^2) * \[Omega]) * Cos[\[Theta]] ^ 2 + 2 * a^4 * ((a^2 + (-2 
    + r) * r) * (a^2 - I * a * m * r - r^2) - (2 * I) * r^2 * (a^2 + r^2)
     * \[Omega]) * Cos[\[Theta]] ^ 4 + 4 * (a^2 + (-2 + r) * r) * (a^2 + r^2) * (r^2 + 
    a^2 * Cos[\[Theta]] ^ 2) ^ 2 - (-1 + r) * r * (a^2 + r^2) * (a^2 + 2 * r^2 +
     a^2 * Cos[2 * \[Theta]]) ^ 2) * Sin[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 * r
     * Cos[\[Theta]] ^ 2) ^ 3), (-2 * (a^2 + (-2 + r) * r) * Cos[\[Theta]] * (a^2 * r^3
     * (-(a^2 * (-10 + r)) + I * a * m * r^2 + r * (-20 + r * (10 + r))) 
    - 2 * a^4 * r * ((-I) * a * m * r^2 + a^2 * (1 + r) + r * (-2 + r - r
    ^2)) * Cos[\[Theta]] ^ 2 + a^6 * (-a^2 + I * a * m * r + r^2) * Cos[\[Theta]] ^ 4) 
    * Sin[\[Theta]]) / (r * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), ((4
     * (a^2 + (-2 + r) * r) * (a^2 - I * a * m * r + (-1 + r) * r)) / r^2
     - 8 * a * m * r * \[Omega] + (a^4 + 2 * r^4 + a^2 * r * (2 + 3 * r)) * \[Omega]^2 
    + a^2 * (a^2 + (-2 + r) * r) * \[Omega]^2 * Cos[2 * \[Theta]] + (384 * (a - I * r) 
    * (a + I * r) * r^3 * (a^2 + (-2 + r) * r)) / (a^2 + 2 * r^2 + a^2 * 
    Cos[2 * \[Theta]]) ^ 3 + (32 * r * (-3 * (a^2 + (-2 + r) * r) * (a^2 + 2 * r
    ^2) - I * r * (a^2 + r^2) ^ 2 * \[Omega])) / (a^2 + 2 * r^2 + a^2 * Cos[2 * 
    \[Theta]]) ^ 2 + (8 * (a^2 + (-2 + r) * r) * (a^2 * (-1 + r) + r^2 - I * a *
     m * r^2) + (8 * I) * r * (a^2 + r^2) ^ 2 * \[Omega]) / (r * (a^2 + 2 * r^2 
    + a^2 * Cos[2 * \[Theta]])) - 2 * (1 + m^2) * (a^2 + (-2 + r) * r) * Csc[\[Theta]] 
    ^ 2) / (2 * (a^2 + r^2) ^ 2), ((a^2 + (-2 + r) * r) * (4 * a * (2 * r
     * (a^2 + (-2 + r) * r) * ((-I) * a^3 * m * r^2 - a^2 * (-16 + r) * r
    ^2 + 5 * r^4 + (2 * I) * a * m * r^4 + a^4 * (2 + r)) + (4 * I) * a^2
     * r^4 * (-a + r) * (a + r) * \[Omega] + a^2 * ((a^2 + (-2 + r) * r) * (a^4 
    - I * a^3 * m * r + (5 * I) * a * m * r^3 + (-12 + r) * r^3 - 2 * a^2
     * r * (3 + 2 * r)) - (2 * I) * r^2 * (a^4 - 4 * a^2 * r^2 + r^4) * \[Omega]
    ) * Cos[\[Theta]] ^ 2 - 2 * a^4 * ((a^2 + (-2 + r) * r) * (a^2 - I * a * m *
     r - r * (1 + r)) - (2 * I) * (a - r) * r^2 * (a + r) * \[Omega]) * Cos[\[Theta]] ^
     4 + a^6 * (a^2 + r * (-2 + r - (2 * I) * r * \[Omega])) * Cos[\[Theta]] ^ 6) * Cot[
    \[Theta]] ^ 3 + 4 * r^3 * ((a^2 + (-2 + r) * r) * (-20 * a^3 + a^3 * r + a *
     (-10 - I * a * m) * r^2 + I * m * r^4) - (2 * I) * a^3 * r^3 * \[Omega]) * 
    Cot[\[Theta]] * Csc[\[Theta]] ^ 2 + a^3 * (a^2 - r) * (a^2 + 2 * r^2 + a^2 * Cos[2 
    * \[Theta]]) ^ 2 * Sin[2 * \[Theta]])) / (2 * (a^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[
    \[Theta]] ^ 2) ^ 3), ConstantArray[0, {Length[r]}], ((2 * I) * (a^2 + (-2 + 
    r) * r) * (-(a * m) + (a^2 + r^2) * \[Omega]) * (-r^2 + a^2 * Cos[\[Theta]] ^ 2)) /
     ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (4 * a * (a^2 + (-
    2 + r) * r) ^ 2 * (-5 * r^2 + a^2 * Cos[\[Theta]] ^ 2) * Cot[\[Theta]]) / ((a^2 + r
    ^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), ((4 * I) * a * r * (a^2 + (-
    2 + r) * r) * (m - a * \[Omega] + a * \[Omega] * Cos[\[Theta]] ^ 2) * Cot[\[Theta]]) / ((a^2 + r^
    2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (-2 * a * (r^4 * (-5 * (a^2 +
     (-2 + r) * r) + I * r * (a^2 + r^2) * \[Omega]) + r^2 * ((a^2 + (-2 + r) * 
    r) * (8 * a^2 + 3 * r^2) - I * r^3 * (a^2 + r^2) * \[Omega]) * Cos[\[Theta]] ^ 2 + 
    a^2 * ((a^2 + (-2 + r) * r) * (a^2 - 10 * r^2) - I * a^2 * r * (a^2 +
     r^2) * \[Omega]) * Cos[\[Theta]] ^ 4 + a^4 * (-a^2 - (-2 + r) * r + I * r * (a^2 +
     r^2) * \[Omega]) * Cos[\[Theta]] ^ 6) * Csc[\[Theta]] ^ 2) / (r * (a^2 + r^2) ^ 2 * (r^2 
    + a^2 * Cos[\[Theta]] ^ 2) ^ 3), (4 * (a^3 + a * (-2 + r) * r) ^ 2 * (-5 * r
    ^2 + a^2 * Cos[\[Theta]] ^ 2) * Cot[\[Theta]]) / (r^2 * (a^2 + r^2) ^ 2 * (r^2 + a^
    2 * Cos[\[Theta]] ^ 2) ^ 3)}, {(2 * (r^4 * (3 * a^7 + I * a^2 * m * r^6 + I 
    * m * (-2 + r) * r^7 + a * r^5 * (4 + r + (3 * I) * r^2 * \[Omega]) + a^5 * 
    r * (-4 + r * (7 + I * r * \[Omega])) + a^3 * r^2 * (-4 + r * (-8 + r + (4 *
     I) * r^2 * \[Omega]))) + a * r^2 * (-9 * a^8 + (4 * I) * a^3 * m * r^6 + (4
     * I) * a * m * (-2 + r) * r^7 - r^7 * (4 + r) - (3 * I) * r^9 * \[Omega] + 
    a^6 * r * (8 + r * (-33 + I * r * \[Omega])) + a^2 * r^4 * (8 + r * (52 + r 
    * (-17 + (3 * I) * r * \[Omega]))) + a^4 * r^2 * (20 + r * (32 + (7 * I) * r
     * (8 * I + r * \[Omega])))) * Cos[\[Theta]] ^ 2 + a^3 * r^2 * ((6 * I) * a^3 * m *
     r^4 + (6 * I) * a * m * (-2 + r) * r^5 + a^6 * (11 - I * r * \[Omega]) + a^
    4 * r^2 * (14 + (3 * I) * r * \[Omega]) + a^2 * r^2 * (-40 + r * (4 + r * (43
     - (3 * I) * r * \[Omega]))) + r^4 * (-4 + r * (-44 + r * (16 - (7 * I) * r 
    * \[Omega])))) * Cos[\[Theta]] ^ 4 + a^5 * ((4 * I) * a^3 * m * r^4 + (4 * I) * a *
     m * (-2 + r) * r^5 + a^6 * (3 - I * r * \[Omega]) + a^2 * r^3 * (-4 + 3 * r
     * (5 - I * r * \[Omega])) + a^4 * r * (-4 + r * (-4 + I * r * \[Omega])) + r^4 * (
    20 + r * (-32 + r * (6 - (5 * I) * r * \[Omega])))) * Cos[\[Theta]] ^ 6 + a^7 * (I 
    * a^3 * m * r^2 + I * a * m * (-2 + r) * r^3 + 2 * a^2 * r * (2 + r) 
    + a^4 * (-3 + I * r * \[Omega]) + r^3 * (-4 + r - I * r^2 * \[Omega])) * Cos[\[Theta]] ^ 8
    )) / (r * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (2 * r^2 *
     ((I * (a * m * (-1 + r) + (a - r) * (a + r) * \[Omega])) / (a^2 + r^2) ^ 2 
    + (384 * r^3 * (a^2 + (-1 + r) * r)) / (a^2 + 2 * r^2 + a^2 * Cos[2 *
     \[Theta]]) ^ 4 - (16 * r * (9 * a^4 + 4 * a^2 * r * (-2 + 7 * r) + r^3 * (-
    20 + 19 * r))) / ((a^2 + r^2) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 
    3) + (8 * ((-I) * a * m * r^2 + r^2 * (-7 + r * (6 + I * r * \[Omega])) + a^
    2 * (1 + r * (8 + I * r * \[Omega])))) / ((a^2 + r^2) * (a^2 + 2 * r^2 + a^2
     * Cos[2 * \[Theta]]) ^ 2) + (-4 + (2 * I) * a * m + 4 * r - (2 * I) * (a^2 
    + r^2) * \[Omega]) / ((a^2 + r^2) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]])))) / 
    a, 2 * r^2 * (((-I) * m * Cot[\[Theta]]) / (a^2 + r^2) ^ 2 + (192 * a * r^2 
    * Sin[2 * \[Theta]]) / (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 4 - (24 * a * (a
    ^2 + 3 * r^2) * Sin[2 * \[Theta]]) / ((a^2 + r^2) * (a^2 + 2 * r^2 + a^2 * Cos[
    2 * \[Theta]]) ^ 3) + (4 * a * (2 * a^2 - I * a * m * r + 2 * r^2 + I * r * 
    (a^2 + r^2) * \[Omega]) * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (a^2 + 2 * r^2 + 
    a^2 * Cos[2 * \[Theta]]) ^ 2) - ((2 * I) * a * r * \[Omega] * Sin[2 * \[Theta]]) / ((a^2 +
     r^2) ^ 2 * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]))), (48 - ((12 * I) * a
    ^3 * m) / r - 4 * a * m * (I * (-4 + r) + 2 * r * \[Omega]) + a^4 * (24 / r^
    2 + \[Omega]^2) + 2 * r * (-16 + r * (2 + \[Omega] * (-2 * I + r^2 * \[Omega]))) + (a^2 * 
    (-72 + r * (28 + \[Omega] * (4 * I + r * (2 + 3 * r) * \[Omega])))) / r + (a^2 + (-
    2 + r) * r) * (a^2 * \[Omega]^2 * Cos[2 * \[Theta]] + (32 * r * (-11 * a^6 + 4 * r^
    6 + 2 * a^2 * r^3 * (2 + 3 * r) - 2 * a^4 * r * (5 + 4 * r) + a^2 * Cos[
    2 * \[Theta]] * (-5 * a^4 - 16 * a^2 * r^2 - 2 * r^3 * (2 + 5 * r) + a^2 * Cos[
    2 * \[Theta]] * (5 * a^2 + 2 * r * (5 + 2 * r) - a^2 * Cos[2 * \[Theta]])))) / (a^2
     + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 4 - 2 * m^2 * Csc[\[Theta]] ^ 2)) / (2 * (a
    ^2 + r^2) ^ 2), (-2 * a * r^2 * (a^2 + (-2 + r) * r) * (r^3 * (-3 * a
    ^2 + (3 - 4 * r) * r) + a^2 * r * (9 * a^2 + r * (-10 + 9 * r)) * Cos[
    \[Theta]] ^ 2 + a^4 * (-1 + r) * Cos[\[Theta]] ^ 4) * Sin[\[Theta]] ^ 2) / ((a^2 + r^2) ^ 
    2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (4 * a * r^2 * (a^2 + (-2 + r) * r
    ) * Cos[\[Theta]] * (9 * a^2 * r^2 + 5 * r^4 + a^2 * Cos[\[Theta]] ^ 2 * (-2 * a^2 
    - 5 * r^2 + a^2 * Cos[2 * \[Theta]])) * Sin[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + 
    a^2 * Cos[\[Theta]] ^ 2) ^ 4), (2 * r^2 * (a * r^3 * (6 * a^3 + I * m * r^3 
    + 2 * a * r * (-3 + 4 * r)) - I * r^6 * (a^2 + r^2) * \[Omega] - a^2 * r * (
    18 * a^4 - I * a * m * r^3 + r^3 * (-6 + r * (8 + I * r * \[Omega])) + a^2 *
     r * (-20 + r * (24 + I * r * \[Omega]))) * Cos[\[Theta]] ^ 2 + a^4 * ((-I) * a * m
     * r^2 + a^2 * (2 + r * (16 + I * r * \[Omega])) + r^2 * (-20 + r * (18 + I 
    * r * \[Omega]))) * Cos[\[Theta]] ^ 4 + a^6 * (-2 - I * a * m + 2 * r + I * (a^2 + 
    r^2) * \[Omega]) * Cos[\[Theta]] ^ 6)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2
    ) ^ 4), (2 * a * r^3 * (r^2 * (-3 * a^2 + (2 - 3 * r) * r) + (9 * a^4
     + 10 * a^2 * (-1 + r) * r + (-2 + r) * r^3) * Cos[\[Theta]] ^ 2 + a^2 * (-11
     * a^2 + (10 - 11 * r) * r) * Cos[\[Theta]] ^ 4)) / ((a^2 + r^2) ^ 2 * (r^2 
    + a^2 * Cos[\[Theta]] ^ 2) ^ 4), ((4 * I) * a * r^2 * ((-9 * I) * a^3 * r^2 
    + m * r^5 - a * r^4 * (5 * I + r * \[Omega]) + a * ((3 * I) * a^4 + 2 * a * 
    m * r^3 + r^4 * (5 * I + r * \[Omega]) - 2 * a^2 * r^2 * (-7 * I + r * \[Omega])) *
     Cos[\[Theta]] ^ 2 + a^3 * (a * m * r - a^2 * (5 * I + r * \[Omega]) + r^2 * (-5 * 
    I + 2 * r * \[Omega])) * Cos[\[Theta]] ^ 4 + a^5 * (2 * I + r * \[Omega]) * Cos[\[Theta]] ^ 6) * 
    Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (2 * a * 
    (r^4 * (3 * a^4 + a^2 * r * (-4 + r * (7 - I * r * \[Omega])) + r^2 * (-4 + 
    r * (-4 + r * (2 - I * r * \[Omega])))) + r^2 * (-9 * a^6 + a^2 * r^2 * (20 
    - r * (4 + 11 * r)) + a^4 * r * (8 + r * (-12 - I * r * \[Omega])) + r^4 * (
    4 + r * (8 + r * (-4 + I * r * \[Omega])))) * Cos[\[Theta]] ^ 2 + a^2 * r^2 * (14 *
     a^4 + 4 * a^2 * r * (-5 + 3 * r) - 2 * r^2 * (10 + (-8 + r) * r) + I
     * r * (a^2 + r^2) ^ 2 * \[Omega]) * Cos[\[Theta]] ^ 4 + a^4 * (4 * a^2 * (1 - 2 * 
    r) * r + a^4 * (-3 + I * r * \[Omega]) + r^3 * (16 + r * (-9 - I * r * \[Omega]))) 
    * Cos[\[Theta]] ^ 6 + a^6 * (3 * a^2 + (-4 + r) * r - I * r * (a^2 + r^2) * 
    \[Omega]) * Cos[\[Theta]] ^ 8) * Csc[\[Theta]] ^ 2) / (r * (a^2 + r^2) ^ 2 * (r^2 + a^2 * 
    Cos[\[Theta]] ^ 2) ^ 4)}, {(2 * a^2 * (a^2 + (-2 + r) * r) ^ 2 * Sin[\[Theta]] ^ 2)
     / (r^2 * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (4 * (a^2 + (-
    2 + r) * r) * (2 * a^2 - I * a * m * r - r^2) - (8 * I) * a^2 * r^2 *
     \[Omega] - 4 * a^2 * (a^2 + r * (-2 + r - (2 * I) * r * \[Omega])) * Cos[\[Theta]] ^ 2) /
     ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[0, {Length[
    r]}], (4 * a * (a^2 + (-2 + r) * r) ^ 2) / (r^2 * (a^2 + r^2) ^ 2 * (
    r^2 + a^2 * Cos[\[Theta]] ^ 2)), ((4 * (a^2 + (-2 + r) * r) * (a^2 + r - I *
     a * m * r - r^2)) / r^2 + (8 * I) * (-a^2 + I * a * m * r + r^2) * \[Omega]
     + (a^4 + 2 * r^4 + a^2 * r * (2 + 3 * r)) * \[Omega]^2 + (a^2 + (-2 + r) * 
    r) * (a^2 * \[Omega]^2 * Cos[2 * \[Theta]] + (8 * (-16 * r^3 + (3 * a^2 - (2 * I) *
     a * m * r - (-6 + r) * r) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]))) / (
    a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 2 - 2 * m^2 * Csc[\[Theta]] ^ 2)) / (2 *
     (a^2 + r^2) ^ 2), (4 * r * (I * a * m + r) * (a^2 + (-2 + r) * r) * 
    Cot[\[Theta]] - 2 * a^2 * (a^2 + r * (-2 + r - (2 * I) * r * \[Omega])) * Sin[2 * \[Theta]
    ]) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]] ^ 2)), (8 * a * (a^2 
    + r * (-2 + r - I * r * \[Omega])) - (4 * I) * m * r * (a^2 + (-2 + r) * r) 
    * Csc[\[Theta]] ^ 2) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), (2 * (a
    ^2 + (-2 + r) * r)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2)), ConstantArray[
    0, {Length[r]}], (2 * (a^2 + (-2 + r) * r) ^ 2 * Csc[\[Theta]] ^ 2) / (r^2 *
     (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2))}, {(-2 * (a^3 + a * (-2 
    + r) * r) ^ 2 * Sin[2 * \[Theta]]) / (r * (a^2 + r^2) ^ 2 * (a^2 + 2 * r^2 +
     a^2 * Cos[2 * \[Theta]])), (2 * a^2 * ((a^2 + (-2 + r) * r) * (a^4 - I * a^
    3 * m * r - (2 * I) * a * m * r^3 + 2 * (10 - 3 * r) * r^3 - a^2 * r 
    * (2 + r)) - (2 * I) * r^2 * (a^2 + r^2) * (a^2 + 2 * r^2) * \[Omega]) * Sin[
    2 * \[Theta]] + a^4 * ((a^2 + (-2 + r) * r) * (a^2 - I * a * m * r - r * (2 
    + 3 * r)) - (2 * I) * r^2 * (a^2 + r^2) * \[Omega]) * Sin[4 * \[Theta]]) / (r * (a^
    2 + r^2) ^ 2 * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 2), (2 * (a^2 + (
    -2 + r) * r) * (a^2 - I * a * m * r - 2 * r^2) - (2 * I) * a^2 * r^2 
    * \[Omega] + (2 * I) * a^2 * r^2 * \[Omega] * Cos[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2
     + a^2 * Cos[\[Theta]] ^ 2)), (-8 * a * (a^2 + (-2 + r) * r) ^ 2 * Cot[\[Theta]]) /
     (r * (a^2 + r^2) ^ 2 * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]])), (a^2 * (
    a^2 + (-2 + r) * r) * (a^2 + r - I * a * m * r - 2 * r^2) * Sin[2 * \[Theta]
    ]) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]] ^ 2)), ((4 * (a^2 + (
    -2 + r) * r) * (a^2 - I * a * m * r + r^2)) / r^2 + (4 * I) * (-a^2 +
     (2 * I) * a * m * r + r^2) * \[Omega] + (a^4 + 2 * r^4 + a^2 * r * (2 + 3 *
     r)) * \[Omega]^2 + (a^2 + (-2 + r) * r) * (a^2 * \[Omega]^2 * Cos[2 * \[Theta]] + (16 * (
    4 * r^3 + (a^2 - I * a * m * r - r * (2 + r)) * (a^2 + 2 * r^2 + a^2 
    * Cos[2 * \[Theta]]))) / (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 2 - 2 * (1 + m
    ^2) * Csc[\[Theta]] ^ 2)) / (2 * (a^2 + r^2) ^ 2), (8 * r * (a^2 + (-2 + r) 
    * r) * Cot[\[Theta]] * (-(a * ((3 * I) * a^3 * m + (3 * I) * a * m * r^2 + 2
     * (-5 + r) * r^2 + 2 * a^2 * (1 + r))) + I * m * (a^2 + r^2) ^ 2 * Csc[
    \[Theta]] ^ 2) + 2 * a^3 * ((a^2 + (-2 + r) * r) * (a^2 + 4 * r + (4 * I) * 
    a * m * r + 6 * r^2) - (2 * I) * r^2 * (a^2 + 2 * r^2) * \[Omega]) * Sin[2 *
     \[Theta]] + a^5 * (a^2 + r * (-2 + r - (2 * I) * r * \[Omega])) * Sin[4 * \[Theta]]) / (4
     * r * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (2 * r * (I *
     a * m + r) * (a^2 + (-2 + r) * r) * Cot[\[Theta]] - a^2 * (a^2 + r * (-2 + 
    r - (2 * I) * r * \[Omega])) * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 *
     r * Cos[\[Theta]] ^ 2)), ((-4 * I) * a * r^2 * \[Omega] + 2 * (a - I * m * r) * (a
    ^2 + (-2 + r) * r) * Csc[\[Theta]] ^ 2) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[
    \[Theta]] ^ 2)), (-4 * (a^2 + (-2 + r) * r) ^ 2 * Cot[\[Theta]] * Csc[\[Theta]] ^ 2) / (r 
    * (a^2 + r^2) ^ 2 * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]))}, {-1 / 4 * (
    a * (a^2 + (-2 + r) * r) ^ 2 * (8 * r^5 + 4 * a^2 * r^2 * (-1 + 2 * r
    ) + a^4 * (5 + 3 * r) + 4 * a^2 * r * (a^2 + r + 2 * r^2) * Cos[2 * \[Theta]
    ] + a^4 * (-5 + r) * Cos[4 * \[Theta]]) * Sin[\[Theta]] ^ 2) / (r * (a^2 + r^2) ^ 2
     * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), (2 * (I * r^2 * (I * (a^2 + (-2 + r
    ) * r) * (a^3 - I * m * r^4 + a * r^2 * (3 + 2 * r)) + a * r * (a^2 +
     r^2) * (a^2 + 3 * r^2) * \[Omega]) + a * ((a^2 + (-2 + r) * r) * (a^4 - 2 *
     a^2 * (-2 + r) * r^2 + (2 * I) * a * m * r^4 + r^4 * (3 + 2 * r)) + 
    (a - I * r) * r * ((-I) * a + r) * (a^4 + 3 * r^4) * \[Omega]) * Cos[\[Theta]] ^ 2 
    + a^3 * (-((a^2 + (-2 + r) * r) * (a^2 + (3 - I * a * m) * r^2 - 2 * 
    r^3)) + I * r * (a^4 - r^4) * \[Omega]) * Cos[\[Theta]] ^ 4)) / (r * (a^2 + r^2) ^ 
    2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (4 * a * r * (a^2 + (-2 + r) * r) 
    * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]])
    ), ((a^2 + (-2 + r) * r) * (2 * (3 * a^8 - (6 * I) * a^7 * m * r + (2
     * I) * a^5 * m * (6 - 11 * r) * r^2 - (32 * I) * a^3 * m * (-1 + r) 
    * r^4 - (16 * I) * a * m * (-2 + r) * r^6 - 32 * (-2 + r) * r^7 + 8 *
     a^2 * r^4 * (-4 + r * (6 + r * (-7 - (2 * I) * \[Omega]))) + 4 * a^4 * r^2 
    * (10 + r + r^2 * (-4 - (2 * I) * \[Omega])) + a^6 * r * (-24 + r - (2 * I) 
    * r * \[Omega])) + a^2 * (3 * a^6 - (16 * I) * a^5 * m * r + (16 * I) * a^3 
    * m * (2 - 3 * r) * r^2 + 16 * a^2 * (2 - 9 * r) * r^3 - (32 * I) * a
     * m * (-2 + r) * r^4 + a^4 * r * (-4 - 31 * r - (2 * I) * r * \[Omega]) + 16
     * r^4 * (4 + r * (10 - 5 * r + (2 * I) * r * \[Omega]))) * Cos[2 * \[Theta]] - 2 *
     a^4 * (3 * a^4 + (2 * I) * a^3 * m * r + (2 * I) * a * m * (-2 + r) 
    * r^2 + a^2 * r * (-24 + 17 * r - (2 * I) * r * \[Omega]) + 4 * r^2 * (10 + 
    r * (-11 + 2 * r - (2 * I) * r * \[Omega]))) * Cos[4 * \[Theta]] - a^6 * (3 * a^2 +
     r * (-4 + r - (2 * I) * r * \[Omega])) * Cos[6 * \[Theta]])) / (16 * r^2 * (a^2 + 
    r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), (2 * (a^2 + (-2 + r) * r) *
     (I * r^3 * (-(a^2 * m * r) + m * (-2 + r) * r^3 + I * a * r * (-3 + 
    r + r^2) + 3 * a * r^3 * \[Omega] + a^3 * (I + r * \[Omega])) + a * r * (5 * a^4 + 
    (3 * I) * a * m * (-1 + r) * r^3 + r^3 * (-3 + r * (1 + r - (3 * I) *
     r * \[Omega])) + a^2 * r * (-10 + r * (8 - 2 * r + (3 * I) * r * \[Omega]))) * Cos[
    \[Theta]] ^ 2 + a^3 * (I * a^3 * m + I * a * m * r^2 * (-2 + 3 * r) - I * a^
    4 * \[Omega] + a^2 * (-1 + r * (-3 - r + I * r * \[Omega])) + r^2 * (10 + r * (-7 +
     2 * r - (4 * I) * r * \[Omega]))) * Cos[\[Theta]] ^ 4 + a^5 * ((-1 + r) * (-1 + I 
    * a * m + r) + I * (a - r) * (a + r) * \[Omega]) * Cos[\[Theta]] ^ 6)) / ((a^2 + r^
    2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), (2 * (a^2 + (-2 + r) * r) * (
    (-1 / 16 * I) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) * (m * (3 * a^4 + 
    8 * r^4 + 8 * a^2 * r * (1 + r)) - 6 * a^3 * r * \[Omega] + 4 * a^2 * (a^2 *
     m + 2 * m * (-1 + r) * r + 2 * a * r * \[Omega]) * Cos[2 * \[Theta]] + a^3 * (a * 
    m - 2 * r * \[Omega]) * Cos[4 * \[Theta]]) * Cot[\[Theta]] + 2 * a^3 * Cos[\[Theta]] * (-4 * r^2 
    + 2 * (a^2 + r^3) * Cos[\[Theta]] ^ 2 + a^2 * (-1 + r) * Cos[\[Theta]] ^ 4) * Sin[\[Theta]
    ] + a * (-5 + r) * r^4 * Sin[2 * \[Theta]])) / ((a^2 + r^2) ^ 2 * (r^2 + a^2
     * Cos[\[Theta]] ^ 2) ^ 3), ((4 * (a^2 - r - I * a * m * r) * (a^2 + (-2 + r
    ) * r)) / r^2 - 8 * a * m * r * \[Omega] + (a^4 + 2 * r^4 + a^2 * r * (2 + 3
     * r)) * \[Omega]^2 + a^2 * (a^2 + (-2 + r) * r) * \[Omega]^2 * Cos[2 * \[Theta]] + (32 * 
    r * (-a^4 + 2 * a^2 * r - 2 * r^3 + r^4 + I * r * (a^2 + r^2) ^ 2 * \[Omega]
    )) / (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 2 + (8 * (a^2 + (-2 + r) * 
    r) * ((-I) * a * m * r^2 + a^2 * (1 + r) - r^2 * (1 + r)) - (8 * I) *
     r * (a^2 + r^2) ^ 2 * \[Omega]) / (r * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]])) 
    - 2 * m^2 * (a^2 + (-2 + r) * r) * Csc[\[Theta]] ^ 2) / (2 * (a^2 + r^2) ^ 2
    ), (-2 * a * (a^2 + (-2 + r) * r) * (-r^3 + r * (5 * a^2 + r^2 + r^3)
     * Cos[\[Theta]] ^ 2 + a^2 * r * (-5 + 2 * r) * Cos[\[Theta]] ^ 4 + a^4 * Cos[\[Theta]] ^ 
    6)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), (2 * r * (I *
     a * m + r) * (a^2 + (-2 + r) * r) * Cot[\[Theta]] - a^2 * (a^2 + r * (-2 + 
    r - (2 * I) * r * \[Omega])) * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 *
     r * Cos[\[Theta]] ^ 2)), ((a^2 + (-2 + r) * r) * (a * (12 * a^6 + (6 * I) *
     a^5 * m * r + (8 * I) * a * m * (-2 + r) * r^4 + (2 * I) * a^3 * m *
     r^2 * (-6 + 7 * r) + a^4 * r * (-31 + 33 * r - (3 * I) * r * \[Omega]) + 4 
    * r^4 * (-2 + r * (-5 + 2 * r - (2 * I) * r * \[Omega])) + 2 * a^2 * r^2 * (
    10 + r * (-23 + 16 * r - (4 * I) * r * \[Omega]))) + 2 * a^3 * (5 * a^4 + I 
    * a^3 * m * r + I * a * m * (-2 + r) * r^2 + a^2 * r * (-13 + 11 * r 
    - (2 * I) * r * \[Omega]) + r^2 * (10 + r * (-17 + 4 * r - (4 * I) * r * \[Omega]))
    ) * Cos[2 * \[Theta]] + a^5 * (2 * a^2 + r * (-3 + r - I * r * \[Omega])) * Cos[4 *
     \[Theta]] - 4 * (a + I * m * r) * (a^2 + (-2 + r) * r) * (a^2 + r^2) ^ 2 * 
    Csc[\[Theta]] ^ 2)) / (2 * r^2 * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) 
    ^ 3)}, {(2 * (a^2 + (-2 + r) * r) ^ 2 * (r^2 * (a^4 + r^4 + 2 * a^2 *
     r * (1 + r)) + a^2 * (a^4 + a^2 * (-4 + r) * r - 2 * r^3) * Cos[\[Theta]] ^
     2 - a^4 * (a^2 + (-4 + r) * r) * Cos[\[Theta]] ^ 4)) / (r^2 * (a^2 + r^2) ^
     2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (4 * (a^2 + (-2 + r) * r) * (r^2 
    * (a^2 + (-3 + r) * r) + a^2 * (a^2 + 3 * r) * Cos[\[Theta]] ^ 2 - a^4 * Cos[
    \[Theta]] ^ 4)) / ((a^2 + r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (2 * a^2 * (
    (a^2 + (-2 + r) * r) * (a^2 - I * a * m * r - r^2) - (2 * I) * r^2 * 
    (a^2 + r^2) * \[Omega]) * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 * r * 
    Cos[\[Theta]] ^ 2)), (-4 * a * (a^2 + (-2 + r) * r) ^ 2 * (-(r^2 * (a^2 + r 
    * (2 + r))) - a^2 * (a^2 - 4 * r) * Cos[\[Theta]] ^ 2 + a^4 * Cos[\[Theta]] ^ 4)) /
     (r^2 * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (2 * (a^2 + 
    (-2 + r) * r) ^ 2 * (r^2 * (a^2 + (-3 + r) * r) + a^2 * (a^2 + 3 * r)
     * Cos[\[Theta]] ^ 2 - a^4 * Cos[\[Theta]] ^ 4)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * 
    Cos[\[Theta]] ^ 2) ^ 2), (2 * a^2 * (a^2 + (-2 + r) * r) * (a^2 - I * a * m 
    * r - r^2) * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]]
     ^ 2)), (4 * a * (a^2 + (-2 + r) * r) * (r^2 * (a^2 + (-3 + r) * r) +
     a^2 * (a^2 + 3 * r) * Cos[\[Theta]] ^ 2 - a^4 * Cos[\[Theta]] ^ 4)) / ((a^2 + r^2)
     ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), ((4 * (a^2 + (-2 + r) * r) * (a
    ^2 - I * a * m * r + (-1 + r) * r)) / r^2 - 8 * a * m * r * \[Omega] + (a^4 
    + 2 * r^4 + a^2 * r * (2 + 3 * r)) * \[Omega]^2 + (a^2 + (-2 + r) * r) * (a^
    2 * \[Omega]^2 * Cos[2 * \[Theta]] + (8 * (-4 * r^3 + (3 * a^2 - (2 * I) * a * m * 
    r + 2 * (-2 + r) * r) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]))) / (a^2 +
     2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 2 - 2 * (2 + m^2) * Csc[\[Theta]] ^ 2)) / (2 
    * (a^2 + r^2) ^ 2), ((4 * I) * m * r * (a^2 + (-2 + r) * r) * (r^2 + 
    a^2 * Cos[2 * \[Theta]]) * Cot[\[Theta]] * Csc[\[Theta]] ^ 2 + 2 * a^3 * (a^2 + r * (-2 + 
    r - (2 * I) * r * \[Omega])) * Sin[2 * \[Theta]]) / ((a^2 + r^2) ^ 2 * (r^3 + a^2 *
     r * Cos[\[Theta]] ^ 2)), (2 * (a^2 + (-2 + r) * r) ^ 2 * (r^2 * (a^2 + 2 * 
    r) + (a^4 + (-2 + r) * r^3 - a^2 * r * (4 + 3 * r)) * Cos[\[Theta]] ^ 2 + a^
    2 * (-3 * a^2 + 4 * r * (1 + r)) * Cos[\[Theta]] ^ 4 + 3 * a^4 * Cos[\[Theta]] ^ 6)
     * Csc[\[Theta]] ^ 4) / (r^2 * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 
    2)}, {(-4 * a^3 * (a^2 + (-2 + r) * r) ^ 2 * Cos[\[Theta]] * (-5 * r^2 + a^2
     * Cos[\[Theta]] ^ 2) * Sin[\[Theta]] ^ 3) / (r^2 * (a^2 + r^2) * (r^2 + a^2 * Cos[
    \[Theta]] ^ 2) ^ 3), (-4 * a^3 * (a^2 + (-2 + r) * r) ^ 2 * Cos[\[Theta]] * (-5 * r
    ^2 + a^2 * Cos[\[Theta]] ^ 2) * Sin[\[Theta]] ^ 3) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 
    * Cos[\[Theta]] ^ 2) ^ 3), (2 * (I * r^4 * ((a^2 + (-2 + r) * r) * ((5 * I) 
    * a^3 + (9 * I) * a * r^2 + m * r^4) + a * r * (a^2 + r^2) * (a^2 + 3
     * r^2) * \[Omega]) + a * r^2 * ((a^2 + (-2 + r) * r) * (8 * a^4 + 7 * a^2 *
     r^2 + 3 * (3 + I * a * m) * r^4) + (3 * I) * r^3 * (a^4 - r^4) * \[Omega]) 
    * Cos[\[Theta]] ^ 2 + a^3 * ((a^2 + (-2 + r) * r) * (a^4 - 9 * a^2 * r^2 - 2
     * r^4 + (3 * I) * a * m * r^4) - I * r * (a^6 + 3 * a^2 * r^4 + 4 * 
    r^6) * \[Omega]) * Cos[\[Theta]] ^ 4 + a^5 * (-((a^2 + (-2 + r) * r) * (a^2 - (1 + 
    I * a * m) * r^2)) + I * r * (a^4 - r^4) * \[Omega]) * Cos[\[Theta]] ^ 6)) / (r * (
    a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), ((a^2 + (-2 + r) * r)
     * ((a^2 - r) * (a^2 + r^2) * (a^3 + 2 * a * r^2 + a^3 * Cos[2 * \[Theta]]) 
    ^ 2 * Sin[2 * \[Theta]] + (64 * r^2 * (a^2 + (-2 + r) * r) * (a^2 + r^2) ^ 3
     * Cot[\[Theta]] + a^2 * ((a^2 + (-2 + r) * r) * (5 * a^6 - (5 * I) * a^5 * 
    m * r - (16 * I) * a^3 * m * r^3 - 112 * a^2 * (-2 + r) * r^3 - (16 *
     I) * a * m * r^5 - 80 * (-2 + r) * r^5 - a^4 * r * (22 + 37 * r)) - 
    (2 * I) * r^2 * (a^2 + r^2) * (5 * a^4 + 16 * a^2 * r^2 + 16 * r^4) *
     \[Omega]) * Sin[2 * \[Theta]] + 4 * a^4 * ((a^2 + (-2 + r) * r) * (a^4 - I * a^3 *
     m * r - (2 * I) * a * m * r^3 - a^2 * r * (2 + r) - 4 * r^3 * (3 + r
    )) - (2 * I) * r^2 * (a^2 + r^2) * (a^2 + 2 * r^2) * \[Omega]) * Sin[4 * \[Theta]] 
    + a^6 * ((a^2 - I * a * m * r - (-2 + r) * r) * (a^2 + (-2 + r) * r) 
    - (2 * I) * r^2 * (a^2 + r^2) * \[Omega]) * Sin[6 * \[Theta]]) / 8)) / (2 * (a^2 + 
    r^2) ^ 2 * (r^3 + a^2 * r * Cos[\[Theta]] ^ 2) ^ 3), ConstantArray[0, {Length[
    r]}], ((2 * I) * (a^2 + (-2 + r) * r) * (r^2 * (-(a^2 * m) + m * (-2 
    + r) * r^2 + a^3 * \[Omega] + 3 * a * r^2 * \[Omega]) + a * Cos[\[Theta]] ^ 2 * (a * m * (
    a^2 + r^2 * (-1 + 2 * r)) - (a^4 + 3 * r^4) * \[Omega] + a^2 * (a * m * (-1 
    + r) + (a - r) * (a + r) * \[Omega]) * Cos[\[Theta]] ^ 2))) / ((a^2 + r^2) ^ 2 * (r
    ^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 2), (2 * (a^2 + (-2 + r) * r) * (r^3 * ((-I)
     * a^3 * m * r^2 + 2 * (-2 + r) * r^4 + a^4 * (10 + r) + a^2 * r * (-
    20 + r * (10 + r))) + a^2 * r * (2 * a^4 * (-1 + r) - (2 * I) * a^3 *
     m * r^2 + I * a * m * r^4 + a^2 * r * (4 + (-12 + r) * r) + r^3 * (20
     + r * (-18 + 5 * r))) * Cos[\[Theta]] ^ 2 + a^4 * (a^4 + a^2 * (2 - I * a *
     m) * r - (4 + a^2) * r^2 + (2 * I) * (I + a * m) * r^3 + 4 * r^4) * 
    Cos[\[Theta]] ^ 4 + a^6 * (-a^2 + I * a * m * r + r^2) * Cos[\[Theta]] ^ 6) * Cot[\[Theta]
    ]) / (r * (a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 3), ((-2 * I) 
    * (a^2 + (-2 + r) * r) * (2 * a^2 * m * r + m * r^4 - 2 * a^3 * r * \[Omega]
     + a^2 * Cos[\[Theta]] ^ 2 * (2 * m * (-1 + r) * r + 4 * a * r * \[Omega] + a * (a 
    * m - 2 * r * \[Omega]) * Cos[\[Theta]] ^ 2)) * Cot[\[Theta]]) / ((a^2 + r^2) ^ 2 * (r^2 +
     a^2 * Cos[\[Theta]] ^ 2) ^ 2), ((4 * (a^2 + (-2 + r) * r) * (a^2 - I * a * 
    m * r + 2 * (-1 + r) * r)) / r^2 + (4 * I) * (a^2 + (2 * I) * a * m *
     r - r^2) * \[Omega] + (a^4 + 2 * r^4 + a^2 * r * (2 + 3 * r)) * \[Omega]^2 + a^2 *
     (a^2 + (-2 + r) * r) * \[Omega]^2 * Cos[2 * \[Theta]] - (384 * (a - I * r) * (a + 
    I * r) * r^3 * (a^2 + (-2 + r) * r)) / (a^2 + 2 * r^2 + a^2 * Cos[2 *
     \[Theta]]) ^ 3 + (32 * r * (3 * (a^2 + (-2 + r) * r) * (a^2 + 2 * r^2) + I 
    * r * (a^2 + r^2) ^ 2 * \[Omega])) / (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 2 
    + (8 * (a^2 + (-2 + r) * r) * ((-I) * a * m * r^2 + a^2 * (1 + r) + r
    ^2 * (-7 + 2 * r)) - (8 * I) * r * (a^2 + r^2) ^ 2 * \[Omega]) / (r * (a^2 +
     2 * r^2 + a^2 * Cos[2 * \[Theta]])) - 2 * (3 + m^2) * (a^2 + (-2 + r) * r) 
    * Csc[\[Theta]] ^ 2) / (2 * (a^2 + r^2) ^ 2), ((a^2 + (-2 + r) * r) * (4 * a
     * (-2 * r * (a^2 + (-2 + r) * r) * (-(a^4 * (-1 + r)) + I * a^3 * m 
    * r^2 - a^2 * (-10 + r) * r^2 - (2 * I) * a * m * r^4 + r^5) + (4 * I
    ) * a^2 * r^4 * (-a + r) * (a + r) * \[Omega] + a^2 * ((a^2 + (-2 + r) * r) 
    * (a^4 - I * a^3 * m * r - 2 * a^2 * (-2 + r) * r + (5 * I) * a * m *
     r^3 + (10 - 3 * r) * r^3) - (2 * I) * r^2 * (a^4 - 4 * a^2 * r^2 + r
    ^4) * \[Omega]) * Cos[\[Theta]] ^ 2 - 2 * a^4 * ((a^2 + r - I * a * m * r) * (a^2 +
     (-2 + r) * r) - (2 * I) * (a - r) * r^2 * (a + r) * \[Omega]) * Cos[\[Theta]] ^ 4 
    + a^6 * (a^2 + r * (-2 + r - (2 * I) * r * \[Omega])) * Cos[\[Theta]] ^ 6) * Cot[\[Theta]]
     ^ 3 + 4 * r^3 * ((a^2 + (-2 + r) * r) * ((-I) * a^2 * m * r^2 + 2 * 
    a * r^3 + I * m * r^4 + a^3 * (10 + r)) - (2 * I) * a^3 * r^3 * \[Omega]) * 
    Cot[\[Theta]] * Csc[\[Theta]] ^ 2 + a^3 * (a^2 - r) * (a^2 + 2 * r^2 + a^2 * Cos[2 
    * \[Theta]]) ^ 2 * Sin[2 * \[Theta]])) / (2 * (a^2 + r^2) ^ 2 * (r^3 + a^2 * r * Cos[
    \[Theta]] ^ 2) ^ 3)}, {(2 * r * (a^2 + (-2 + r) * r) * (r^2 * (-3 * a^6 + a^
    2 * (-8 + r) * r^4 + r^7 - a^4 * r * (2 + 9 * r)) + a^2 * Cos[\[Theta]] ^ 2 
    * (9 * a^6 + r^6 * (8 + 3 * r) + a^4 * r * (10 + 23 * r) + a^2 * r^3 
    * (4 + r * (16 + 3 * r)) + a^2 * Cos[\[Theta]] ^ 2 * (-4 * a^4 + a^2 * r * (
    1 + r) * (-20 + 3 * r) + r^3 * (-2 + r * (-7 + 3 * r)) + a^2 * (a^2 *
     (-5 + r) + r * (10 + (-3 + r) * r)) * Cos[\[Theta]] ^ 2))) * Sin[\[Theta]] ^ 2) / 
    ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (4 * r^2 * (r^3 * (
    -3 * a^4 + 3 * a^2 * (1 - 2 * r) * r + (-3 + r) * r^4) + a^2 * r * (9
     * a^4 + 2 * a^2 * r * (-5 + 7 * r) + 3 * r^3 * (-1 + r + r^2)) * Cos[
    \[Theta]] ^ 2 - a^4 * (a^2 * (1 + 4 * r) + r^2 * (-10 + (8 - 3 * r) * r)) * 
    Cos[\[Theta]] ^ 4 + a^6 * (-1 + r) ^ 2 * Cos[\[Theta]] ^ 6) * Sin[\[Theta]] ^ 2) / ((a^2 +
     r^2) * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (-4 * r^2 * Cos[\[Theta]] * (9 * a^4 
    * r^2 + 10 * a^2 * r^4 + r^7 - a^2 * (3 * a^4 + 10 * a^2 * r^2 + (10 
    - 3 * r) * r^4) * Cos[\[Theta]] ^ 2 + a^4 * (4 * a^2 + r^2 + 3 * r^3) * Cos[
    \[Theta]] ^ 4 + a^6 * (-1 + r) * Cos[\[Theta]] ^ 6) * Sin[\[Theta]]) / ((a^2 + r^2) * (r^2
     + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (4 * (r^4 * (-3 * a^7 + I * a^2 * m * r^6 
    + I * m * (-2 + r) * r^7 + a^5 * r * (4 + r * (-12 + I * r * \[Omega])) + a 
    * r^5 * (20 + r * (-9 + r + (3 * I) * r * \[Omega])) + a^3 * r^2 * (4 + r * 
    (14 + r * (-20 + r + (4 * I) * r * \[Omega])))) + a * r^2 * (9 * a^8 + (4 * 
    I) * a^3 * m * r^6 + (4 * I) * a * m * (-2 + r) * r^7 + a^6 * r * (-8
     + r * (23 + I * r * \[Omega])) + r^7 * (-20 + r * (9 - r - (3 * I) * r * \[Omega])
    ) + a^2 * r^4 * (-8 + r * (8 + r * (11 + 2 * r + (3 * I) * r * \[Omega]))) +
     a^4 * r^2 * (-20 + r * (-16 + r * (8 + 3 * r + (7 * I) * r * \[Omega])))) *
     Cos[\[Theta]] ^ 2 + a^3 * r^2 * ((6 * I) * a^3 * m * r^4 + (6 * I) * a * m 
    * (-2 + r) * r^5 + a^6 * (-10 - I * r * \[Omega]) + a^4 * r * (2 + r * (-41 
    + 3 * r + (3 * I) * r * \[Omega])) + a^2 * r^2 * (40 + r * (28 + r * (-10 - 
    (3 * I) * r * \[Omega]))) + r^4 * (4 + r * (-22 + r * (9 - 3 * r - (7 * I) *
     r * \[Omega])))) * Cos[\[Theta]] ^ 4 + a^5 * ((4 * I) * a^3 * m * r^4 + (4 * I) * 
    a * m * (-2 + r) * r^5 + a^6 * (3 - I * r * \[Omega]) + a^4 * r * (-4 + r^2 
    + I * r^2 * \[Omega]) + a^2 * r^3 * (8 + r * (19 - 2 * r - (3 * I) * r * \[Omega]))
     + r^4 * (-20 + r * (-16 + r * (14 - 3 * r - (5 * I) * r * \[Omega])))) * Cos[
    \[Theta]] ^ 6 + a^7 * (I * a^3 * m * r^2 + I * a * m * (-2 + r) * r^3 + a^2 
    * r * (4 + r - r^2) + a^4 * (-3 + I * r * \[Omega]) + r^3 * (-2 + r * (2 - r
     - I * r * \[Omega]))) * Cos[\[Theta]] ^ 8)) / (r * (a^2 + r^2) ^ 2 * (r^2 + a^2 * 
    Cos[\[Theta]] ^ 2) ^ 4), (2 * r^2 * (a^2 + (-2 + r) * r) * (r^3 * (-3 * a^4 
    + 3 * a^2 * (1 - 2 * r) * r + (-3 + r) * r^4) + a^2 * r * (9 * a^4 + 
    2 * a^2 * r * (-5 + 7 * r) + 3 * r^3 * (-1 + r + r^2)) * Cos[\[Theta]] ^ 2 -
     a^4 * (a^2 * (1 + 4 * r) + r^2 * (-10 + (8 - 3 * r) * r)) * Cos[\[Theta]] ^
     4 + a^6 * (-1 + r) ^ 2 * Cos[\[Theta]] ^ 6) * Sin[\[Theta]] ^ 2) / ((a^2 + r^2) ^ 
    2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (-4 * r^2 * (a^2 + (-2 + r) * r) *
     Cos[\[Theta]] * (9 * a^4 * r^2 + 10 * a^2 * r^4 + r^7 - a^2 * (3 * a^4 + 10
     * a^2 * r^2 + (10 - 3 * r) * r^4) * Cos[\[Theta]] ^ 2 + a^4 * (4 * a^2 + r^
    2 + 3 * r^3) * Cos[\[Theta]] ^ 4 + a^6 * (-1 + r) * Cos[\[Theta]] ^ 6) * Sin[\[Theta]]) / 
    ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (4 * r^2 * (r^3 * (
    -3 * a^5 - I * a^2 * m * r^3 + I * m * (-2 + r) * r^5 + a * r^4 * (-3
     + r + (3 * I) * r * \[Omega]) + a^3 * r * (3 + r * (-6 + I * r * \[Omega]))) + a *
     r * (9 * a^6 - I * a^3 * m * r^3 + I * a * m * r^5 * (-5 + 4 * r) + 
    r^6 * (3 - r - (3 * I) * r * \[Omega]) + a^4 * r * (-10 + r * (17 + I * r * 
    \[Omega])) + 3 * a^2 * r^3 * (-2 + r * (3 + r + (2 * I) * r * \[Omega]))) * Cos[\[Theta]] 
    ^ 2 + a^3 * (I * a^3 * m * r^2 + I * a * m * r^4 * (-5 + 6 * r) - 3 *
     r^4 * (-1 + r + r^2) - (7 * I) * r^6 * \[Omega] + a^4 * (-1 + r * (-13 - I 
    * r * \[Omega])) + a^2 * r^2 * (20 + r * (-22 + 3 * r + (4 * I) * r * \[Omega]))) *
     Cos[\[Theta]] ^ 4 + a^5 * (I * a^3 * m + I * a * m * r^2 * (-3 + 4 * r) - I
     * a^4 * \[Omega] + a^2 * (2 + r * (2 + r + (2 * I) * r * \[Omega])) + r^2 * (-10 +
     r * (8 - 3 * r - (5 * I) * r * \[Omega]))) * Cos[\[Theta]] ^ 6 + I * a^7 * ((a * m
     + I * (-1 + r)) * (-1 + r) + (a - r) * (a + r) * \[Omega]) * Cos[\[Theta]] ^ 8)) /
     ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (2 * r^2 * (r^3 * 
    (3 * a^4 + 2 * r^4 + a^2 * r * (-2 + 5 * r)) + r * (-9 * a^6 + 5 * a^
    4 * (2 - 3 * r) * r + (-2 + r) * r^6 + a^2 * r^3 * (4 + (-6 + r) * r)
    ) * Cos[\[Theta]] ^ 2 + a^2 * r * (16 * a^4 + r^3 * (-2 + r + 3 * r^2) + a^2
     * r * (-20 + r * (23 + 3 * r))) * Cos[\[Theta]] ^ 4 + a^4 * r * (-5 + 3 * r
    ) * (a^2 + (-2 + r) * r) * Cos[\[Theta]] ^ 6 + a^6 * (a^2 + (-2 + r) * r) * 
    Cos[\[Theta]] ^ 8)) / ((a^2 + r^2) ^ 2 * (r^2 + a^2 * Cos[\[Theta]] ^ 2) ^ 4), (4 *
     r^2 * ((-I) * m * Cot[\[Theta]] + (a * (-96 * r^2 * (a^2 + r^2) ^ 2 + 12 * 
    (a^2 + r^2) * (a^2 + 5 * r^2) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) + 
    (4 * I) * (-(a * m * r) + a^2 * (2 * I + r * \[Omega]) + r^2 * (2 * I + r * 
    \[Omega])) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 2 + (1 + r * (-1 - (2 * I)
     * \[Omega])) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 3) * Sin[2 * \[Theta]]) / (a^2
     + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 4)) / (a^2 + r^2) ^ 2, (64 - ((12 * 
    I) * a^3 * m) / r - 4 * a * m * (I * (-4 + r) + 2 * r * \[Omega]) + a^4 * (24
     / r^2 + \[Omega]^2) + 2 * r * (-22 + r * (2 + \[Omega] * (-4 * I + r^2 * \[Omega]))) + (a
    ^2 * (-84 + r * (36 + \[Omega] * (8 * I + r * (2 + 3 * r) * \[Omega])))) / r + a^2 
    * (a^2 + (-2 + r) * r) * \[Omega]^2 * Cos[2 * \[Theta]] + (8 * (-96 * r^3 * (a^2 + 
    (-2 + r) * r) * (a^2 + r^2) * (a^2 + r + r^2) + 4 * r * (a^2 + (-2 + 
    r) * r) * (9 * a^4 + 2 * a^2 * r * (5 + 17 * r) + r^3 * (22 + 25 * r)
    ) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) + 2 * r * (a^4 * (-25 + (4 * I
    ) * r * \[Omega]) + 4 * a^2 * r * (8 + r * (-13 + (2 * I) * r * \[Omega])) + r^2 * 
    (20 + r * (52 + r * (-27 + (4 * I) * r * \[Omega])))) * (a^2 + 2 * r^2 + a^2
     * Cos[2 * \[Theta]]) ^ 2 + ((a^4 * (6 - (2 * I) * r * \[Omega]) + r^3 * (-24 + r *
     (8 + r - (2 * I) * r * \[Omega])) + a^2 * r * (-8 + r * (16 + r - (4 * I) *
     r * \[Omega]))) * (a^2 + 2 * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 3) / r)) / (a^2 + 2 
    * r^2 + a^2 * Cos[2 * \[Theta]]) ^ 4 - 2 * (-2 + m^2) * (a^2 + (-2 + r) * r)
     * Csc[\[Theta]] ^ 2) / (2 * (a^2 + r^2) ^ 2)}};


d1SecP1[h_] :=
  1 / (2 h)


d1SecM1[h_] :=
  -1 / (2 h)


d2SecP1[h_] :=
  1 / h^2


d2SecM1[h_] :=
  1 / h^2


d2Sec0[h_] :=
  -2 / h^2


posToI[pos_, iMax_] :=
  Mod[pos - 1, iMax] + 1


posToJ[pos_, iMax_] :=
  Quotient[pos - 1, iMax] + 1


colToPos[col_, n_] :=
  Quotient[col - 1, n] + 1


ijToPos[i_, j_, iMax_] :=
  i + (j - 1) iMax


lPosToCol[l_, pos_, n_] :=
  l + (pos - 1) n


couplingMatrixSec[m_, a_, r0_, rStarList_, rSubrPlusList_, thetaList_
  ] :=
  Module[{n, M, v, \[CapitalOmega], rPlus, iMax, jMax, i, j, mat, row, col, rSubrPlus,
     \[Theta], \[CapitalDelta]rStar, \[CapitalDelta]\[Theta], \[Omega], rSlice, \[Theta]Slice, Aar, Bar, Car, Cdiags, Cmat, leftAdiags,
     leftAmat, rightAdiags, rightAmat, leftBdiags, leftBmat, rightBdiags,
     rightBmat, \[Theta]2coeff, \[Theta]2mat, rStar2coeff, rStar2mat, \[Theta]BCmat, rBCmat, drcls,
     nemns},
    Parallelize[
      n = 10;
      M = 1;
      rPlus = M + Sqrt[M^2 - a^2];
      v = 1 / Sqrt[r0];
      \[CapitalOmega] = v^3 / (1 + a v^3);
      \[Omega] = m \[CapitalOmega];
      iMax = Length[rStarList];
      jMax = Length[thetaList];
      \[CapitalDelta]rStar = (rStarList[[-1]] - rStarList[[1]]) / (iMax - 1);
      \[CapitalDelta]\[Theta] = (thetaList[[-1]] - thetaList[[1]]) / (jMax - 1);
      rSlice = rPlus + rSubrPlusList[[2 ;; -2]];
      \[Theta]Slice = thetaList[[2 ;; -2]];
      Aar = Join[{ConstantArray[0, {n, n, iMax}]}, Table[ArrayPad[Afunc[
        m, a, \[Omega], rSlice, \[Theta]], 1][[2 ;; -2, 2 ;; -2]], {\[Theta], \[Theta]Slice}], {ConstantArray[
        0, {n, n, iMax}]}];
      Bar = Join[{ConstantArray[0, {n, n, iMax}]}, Table[ArrayPad[Bfunc[
        m, a, \[Omega], rSlice, \[Theta]], 1][[2 ;; -2, 2 ;; -2]], {\[Theta], \[Theta]Slice}], {ConstantArray[
        0, {n, n, iMax}]}];
      Car = Join[{ConstantArray[0, {n, n, iMax}]}, Table[ArrayPad[Cfunc[
        m, a, \[Omega], rSlice, \[Theta]], 1][[2 ;; -2, 2 ;; -2]], {\[Theta], \[Theta]Slice}], {ConstantArray[
        0, {n, n, iMax}]}];
      Cdiags = Table[SparseArray[{k_} :> Boole[Mod[k + (Abs[l - p] + 
        l - p) / 2 + n - l, n] == 0] Car[[posToJ[colToPos[k + (Abs[l - p] + l
         - p) / 2, n], iMax], l, p, posToI[colToPos[k + (Abs[l - p] + l - p) 
        / 2, n], iMax]]], {iMax * jMax * n - Abs[l - p]}, 0], {l, 1, n}, {p, 
        1, n}];
      Cmat = Sum[DiagonalMatrix[Sum[Cdiags[[iter + (Abs[shift] + shift
        ) / 2, iter + (Abs[shift] - shift) / 2]], {iter, 1, n - Abs[shift]}],
         -shift], {shift, 1 - n, n - 1}];
      leftAdiags = Table[SparseArray[{k_} :> Boole[Mod[k + n + l - p 
        + n - l, n] == 0] d1SecM1[\[CapitalDelta]rStar] Aar[[posToJ[colToPos[k + n + l - p,
         n], iMax], l, p, posToI[colToPos[k + n + l - p, n], iMax]]], {iMax *
         jMax * n - n - l + p}, 0], {l, 1, n}, {p, 1, n}];
      leftAmat = Sum[DiagonalMatrix[Sum[leftAdiags[[iter + (Abs[shift
        ] + shift) / 2, iter + (Abs[shift] - shift) / 2]], {iter, 1, n - Abs[
        shift]}], -shift - n], {shift, 1 - n, n - 1}];
      rightAdiags = Table[SparseArray[{k_} :> Boole[Mod[k + n - l, n]
         == 0] d1SecP1[\[CapitalDelta]rStar] Aar[[posToJ[colToPos[k, n], iMax], l, p, posToI[
        colToPos[k, n], iMax]]], {iMax * jMax * n - n + l - p}, 0], {l, 1, n},
         {p, 1, n}];
      rightAmat = Sum[DiagonalMatrix[Sum[rightAdiags[[iter + (Abs[shift
        ] + shift) / 2, iter + (Abs[shift] - shift) / 2]], {iter, 1, n - Abs[
        shift]}], -shift + n], {shift, 1 - n, n - 1}];
      leftBdiags = Table[SparseArray[{k_} :> Boole[Mod[k + n + l - p 
        + n iMax - l, n] == 0] d1SecM1[\[CapitalDelta]\[Theta]] Bar[[posToJ[colToPos[k + n + l - p
         + n iMax, n], iMax], l, p, posToI[colToPos[k + n + l - p + n iMax, n
        ], iMax]]], {iMax * jMax * n - n - l + p - n iMax}, 0], {l, 1, n}, {p,
         1, n}];
      leftBmat = Sum[DiagonalMatrix[Sum[leftBdiags[[iter + (Abs[shift
        ] + shift) / 2, iter + (Abs[shift] - shift) / 2]], {iter, 1, n - Abs[
        shift]}], -shift - n - n iMax], {shift, 1 - n, n - 1}];
      rightBdiags = Table[SparseArray[{k_} :> Boole[Mod[k + n - l, n]
         == 0] d1SecP1[\[CapitalDelta]\[Theta]] Bar[[posToJ[colToPos[k, n], iMax], l, p, posToI[colToPos[
        k, n], iMax]]], {iMax * jMax * n - n + l - p - n iMax}, 0], {l, 1, n},
         {p, 1, n}];
      rightBmat = Sum[DiagonalMatrix[Sum[rightBdiags[[iter + (Abs[shift
        ] + shift) / 2, iter + (Abs[shift] - shift) / 2]], {iter, 1, n - Abs[
        shift]}], -shift + n + n iMax], {shift, 1 - n, n - 1}];
      \[Theta]2coeff = Join[{ConstantArray[0, {iMax}]}, Table[ArrayPad[(rSlice
        ^2 - 2 M rSlice + a^2) / (rSlice^2 + a^2) ^ 2, 1], {\[Theta], \[Theta]Slice}], {ConstantArray[
        0, {iMax}]}];
      \[Theta]2mat = DiagonalMatrix[SparseArray[{k_} :> d2Sec0[\[CapitalDelta]\[Theta]] \[Theta]2coeff[[
        posToJ[colToPos[k, n], iMax], posToI[colToPos[k, n], iMax]]], {iMax *
         jMax * n}, 0], 0] + DiagonalMatrix[SparseArray[{k_} :> d2SecP1[\[CapitalDelta]\[Theta]] \[Theta]2coeff
        [[posToJ[colToPos[k, n], iMax], posToI[colToPos[k, n], iMax]]], {iMax
         * jMax * n - n - n iMax}, 0], n + n iMax] + DiagonalMatrix[SparseArray[
        {k_} :> d2SecM1[\[CapitalDelta]\[Theta]] \[Theta]2coeff[[posToJ[colToPos[k + n + n iMax, n], iMax
        ], posToI[colToPos[k + n + n iMax, n], iMax]]], {iMax * jMax * n - n 
        - n iMax}, 0], -n - n iMax];
      rStar2coeff = Join[{ConstantArray[0, {iMax}]}, Table[ArrayPad[ConstantArray[
        1, {Length[rSlice]}], 1], {\[Theta], \[Theta]Slice}], {ConstantArray[0, {iMax}]}];
      rStar2mat = DiagonalMatrix[SparseArray[{k_} :> d2Sec0[\[CapitalDelta]rStar] rStar2coeff
        [[posToJ[colToPos[k, n], iMax], posToI[colToPos[k, n], iMax]]], {iMax
         * jMax * n}, 0], 0] + DiagonalMatrix[SparseArray[{k_} :> d2SecP1[\[CapitalDelta]rStar
        ] rStar2coeff[[posToJ[colToPos[k, n], iMax], posToI[colToPos[k, n], iMax
        ]]], {iMax * jMax * n - n}, 0], n] + DiagonalMatrix[SparseArray[{k_} 
        :> d2SecM1[\[CapitalDelta]rStar] rStar2coeff[[posToJ[colToPos[k + n, n], iMax], posToI[
        colToPos[k + n, n], iMax]]], {iMax * jMax * n - n}, 0], -n];
      drcls =
        If[m == 0,
          {0, 0, 1, 1, 0, 1, 1, 1, 1, 1}
          ,
          If[m == 1,
            {1, 1, 0, 0, 1, 0, 0, 1, 1, 1}
            ,
            If[m == 2,
              {1, 1, 1, 1, 1, 1, 1, 0, 0, 0}
              ,
              {1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
            ]
          ]
        ];
      nemns = 2 + BitNot[drcls];
      \[Theta]BCmat = SparseArray[Join[Flatten[Table[{j + (i - 1) * n, j + (
        i - 1) * n} -> -3 / (2 \[CapitalDelta]\[Theta]) nemns[[j]] + drcls[[j]], {j, 1, n}, {i, 1,
         iMax}]], Flatten[Table[{j + (i - 1) * n, j + (i - 1) * n + iMax n} ->
         4 / (2 \[CapitalDelta]\[Theta]) nemns[[j]], {j, 1, n}, {i, 1, iMax}]], Flatten[Table[{j +
         (i - 1) * n, j + (i - 1) * n + 2 iMax n} -> 4 / (2 \[CapitalDelta]\[Theta]) nemns[[j]], {
        j, 1, n}, {i, 1, iMax}]], Flatten[Table[{iMax * jMax * n - iMax * n +
         j + (i - 1) * n, iMax * jMax * n - iMax * n + j + (i - 1) * n} -> 3 
        / (2 \[CapitalDelta]\[Theta]) nemns[[j]] + drcls[[j]], {j, 1, n}, {i, 1, iMax}]], Flatten[
        Table[{iMax * jMax * n - iMax * n + j + (i - 1) * n, iMax * jMax * n 
        - 2 iMax * n + j + (i - 1) * n} -> -4 / (2 \[CapitalDelta]\[Theta]) nemns[[j]], {j, 1, 
        n}, {i, 1, iMax}]], Flatten[Table[{iMax * jMax * n - iMax * n + j + (
        i - 1) * n, iMax * jMax * n - 3 iMax * n + j + (i - 1) * n} -> 1 / (2
         \[CapitalDelta]\[Theta]) nemns[[j]], {j, 1, n}, {i, 1, iMax}]]], {iMax * jMax * n, iMax *
         jMax * n}, 0];
      (*\[Theta]BCmat =
        If[m == 0,
          SparseArray[{Band[{1, 1}, {iMax * n, iMax * n}] -> -3 / (2 
            \[CapitalDelta]\[Theta]), Band[{1, 1 + iMax n}, {iMax * n, iMax * n + iMax n}] -> 4 / (2 \[CapitalDelta]\[Theta]
            ), Band[{1, 1 + 2 iMax n}, {iMax * n, iMax * n + 2 iMax n}] -> -1 / (
            2 \[CapitalDelta]\[Theta]), Band[{iMax * jMax * n + 1 - iMax * n, iMax * jMax * n + 1 - iMax
             * n}, {iMax * jMax * n, iMax * jMax * n}] -> 3 / (2 \[CapitalDelta]\[Theta]), Band[{iMax 
            * jMax * n + 1 - iMax * n, iMax * jMax * n + 1 - 2 iMax * n}, {iMax *
             jMax * n, iMax * jMax * n - iMax * n}] -> -4 / (2 \[CapitalDelta]\[Theta]), Band[{iMax * 
            jMax * n + 1 - iMax * n, iMax * jMax * n + 1 - 3 iMax * n}, {iMax * jMax
             * n, iMax * jMax * n - 2 iMax * n}] -> 1 / (2 \[CapitalDelta]\[Theta])}, {iMax * jMax * n,
             iMax * jMax * n}, 0]
          ,
          SparseArray[{Band[{1, 1}, {iMax * n, iMax * n}] -> 1, Band[
            {iMax * jMax * n + 1 - iMax * n, iMax * jMax * n + 1 - iMax * n}, {iMax
             * jMax * n, iMax * jMax * n}] -> 1}, {iMax * jMax * n, iMax * jMax *
             n}, 0]
        ];*)
      rBCmat =
        SparseArray[Flatten[Table[{Band[{lPosToCol[1, ijToPos[1, j, iMax
          ], n], lPosToCol[1, ijToPos[1, j, iMax], n]}, {lPosToCol[n, ijToPos[1,
           j, iMax], n], lPosToCol[n, ijToPos[1, j, iMax], n]}] -> I \[Omega] - 3 / (2
           \[CapitalDelta]rStar), Band[{lPosToCol[1, ijToPos[1, j, iMax], n], lPosToCol[1, ijToPos[
          2, j, iMax], n]}, {lPosToCol[n, ijToPos[1, j, iMax], n], lPosToCol[n,
           ijToPos[2, j, iMax], n]}] -> 2 / \[CapitalDelta]rStar, Band[{lPosToCol[1, ijToPos[
          1, j, iMax], n], lPosToCol[1, ijToPos[3, j, iMax], n]}, {lPosToCol[n,
           ijToPos[1, j, iMax], n], lPosToCol[n, ijToPos[3, j, iMax], n]}] -> -
          1 / (2 \[CapitalDelta]rStar)}, {j, 2, jMax - 1}]], {iMax * jMax * n, iMax * jMax * 
          n}, 0] +
          If[m == 0,
            SparseArray[Table[Band[{lPosToCol[1, ijToPos[iMax, j, iMax
              ], n], lPosToCol[1, ijToPos[iMax, j, iMax], n]}, {lPosToCol[n, ijToPos[
              iMax, j, iMax], n], lPosToCol[n, ijToPos[iMax, j, iMax], n]}] -> 1, {
              j, 2, jMax - 1}], {iMax * jMax * n, iMax * jMax * n}, 0]
            ,
            SparseArray[Flatten[Table[{Band[{lPosToCol[1, ijToPos[iMax,
               j, iMax], n], lPosToCol[1, ijToPos[iMax, j, iMax], n]}, {lPosToCol[n,
               ijToPos[iMax, j, iMax], n], lPosToCol[n, ijToPos[iMax, j, iMax], n]}
              ] -> -(I + \[CapitalDelta]rStar \[Omega]) (2 I + \[CapitalDelta]rStar \[Omega]) / \[CapitalDelta]rStar^2, Band[{lPosToCol[1, 
              ijToPos[iMax, j, iMax], n], lPosToCol[1, ijToPos[iMax - 1, j, iMax], 
              n]}, {lPosToCol[n, ijToPos[iMax, j, iMax], n], lPosToCol[n, ijToPos[iMax
               - 1, j, iMax], n]}] -> (-5 + 4 I \[CapitalDelta]rStar \[Omega]) / \[CapitalDelta]rStar^2, Band[{lPosToCol[
              1, ijToPos[iMax, j, iMax], n], lPosToCol[1, ijToPos[iMax - 2, j, iMax
              ], n]}, {lPosToCol[n, ijToPos[iMax, j, iMax], n], lPosToCol[n, ijToPos[
              iMax - 2, j, iMax], n]}] -> (4 - I \[CapitalDelta]rStar \[Omega]) / \[CapitalDelta]rStar^2, Band[{lPosToCol[
              1, ijToPos[iMax, j, iMax], n], lPosToCol[1, ijToPos[iMax - 3, j, iMax
              ], n]}, {lPosToCol[n, ijToPos[iMax, j, iMax], n], lPosToCol[n, ijToPos[
              iMax - 3, j, iMax], n]}] -> -1 / \[CapitalDelta]rStar^2}, {j, 2, jMax - 1}]], {iMax
               * jMax * n, iMax * jMax * n}, 0]
          ];
      mat = Cmat + leftAmat + rightAmat + leftBmat + rightBmat + \[Theta]2mat
         + rStar2mat + \[Theta]BCmat + rBCmat;
    ];
    Return[mat];
  ]


testMatrix[m_, a_, r0_, \[CapitalDelta]rStar_, \[CapitalDelta]\[Theta]_, rStarMax_, rStarMin_] := Module[{rStarList,
   rSubrPlusList, thetaList, mat},
  rStarList = Table[N[rStar], {rStar, rStarMin, rStarMax, \[CapitalDelta]rStar}]; 
  rSubrPlusList = getrSubrPlusList[a, rStarList]; 
  thetaList = Table[N[\[Theta]], {\[Theta], 0, 2 \[Pi], \[CapitalDelta]\[Theta]}]; 
  mat = couplingMatrixSec[m, N[a], N[r0], rStarList, rSubrPlusList, thetaList]; 
  Return[mat];
]


sourceVectorPoint[m_, a_, r0_, rStarList_, rSubrPlusList_, thetaList_, iSourceMin_,
   iSourceMax_, jSourceMin_, jSourceMax_] :=
  Module[{n, iMax, jMax, vec, l, i, j},
    n = 10;
    iMax = Length[rStarList];
    jMax = Length[thetaList];
    vec = SparseArray[{}, {n iMax jMax}, 0];
    l = 1;
    i = Floor[(iSourceMin + iSourceMax)/2];
    j = Floor[(jSourceMin + jSourceMax)/2];
    vec[[lPosToCol[l, ijToPos[i, j, iMax], n]]] = 1;
    i = Ceiling[(iSourceMin + iSourceMax)/2];
    j = Floor[(jSourceMin + jSourceMax)/2];
    vec[[lPosToCol[l, ijToPos[i, j, iMax], n]]] = 1;
    i = Floor[(iSourceMin + iSourceMax)/2];
    j = Ceiling[(jSourceMin + jSourceMax)/2];
    vec[[lPosToCol[l, ijToPos[i, j, iMax], n]]] = 1;
    i = Ceiling[(iSourceMin + iSourceMax)/2];
    j = Ceiling[(jSourceMin + jSourceMax)/2];
    vec[[lPosToCol[l, ijToPos[i, j, iMax], n]]] = 1;
    Return[vec];
  ]


sourceVector[m_, a_, r0_, rStarList_, rSubrPlusList_, thetaList_, iSourceMin_,
   iSourceMax_, jSourceMin_, jSourceMax_] :=
  Module[{n, M, v, \[CapitalOmega], iMax, jMax, i, j, vec, row, rCoeff, \[Theta], \[CapitalDelta]rStar, 
    \[CapitalDelta]\[Theta], \[Omega], rPlus, r, data},
    n = 10;
    M = 1;
    v = 1 / Sqrt[r0];
    \[CapitalOmega] = v^3 / (1 + a v^3);
    \[Omega] = m \[CapitalOmega];
    rPlus = 1 + Sqrt[1 - a^2];
    iMax = Length[rStarList];
    jMax = Length[thetaList];
    \[CapitalDelta]rStar = (rStarList[[-1]] - rStarList[[1]]) / (iMax - 1);
    \[CapitalDelta]\[Theta] = (thetaList[[-1]] - thetaList[[1]]) / (jMax - 1);
    vec = SparseArray[{}, {n iMax jMax}, 0];
    Do[
      \[Theta] = thetaList[[j]];
      Do[
        r = rSubrPlusList[[i]] + rPlus;
        data = Seffm[m, a, r, \[Theta]];
        Do[vec[[lPosToCol[l, ijToPos[i, j, iMax], n]]] = data[[l]];, 
          {l, 1, n}]
        ,
        {i, iSourceMin, iSourceMax}
      ]
      ,
      {j, jSourceMin, jSourceMax}
    ];
    Do[
      rCoeff = {rSubrPlusList[[i]] + rPlus};
      r = rSubrPlusList[[i]] + rPlus;
      \[Theta] = thetaList[[jSourceMin]];
      data = (d1SecM1[\[CapitalDelta]\[Theta]] Bfunc[m, a, \[Omega], rCoeff, \[Theta]][[All, All, 1]] + 
        d2SecM1[\[CapitalDelta]\[Theta]] (rCoeff[[1]] ^ 2 - 2 M rCoeff[[1]] + a^2) / (rCoeff[[1]] 
        ^ 2 + a^2) ^ 2 IdentityMatrix[n]) . PsiPm[m, a, r, thetaList[[jSourceMin
         - 1]]];
      Do[
        row = lPosToCol[l, ijToPos[i, jSourceMin, iMax], n];
        vec[[row]] = vec[[row]] + data[[l]];
        ,
        {l, 1, n}
      ];
      \[Theta] = thetaList[[jSourceMax]];
      data = (d1SecP1[\[CapitalDelta]\[Theta]] Bfunc[m, a, \[Omega], rCoeff, \[Theta]][[All, All, 1]] + 
        d2SecP1[\[CapitalDelta]\[Theta]] (rCoeff[[1]] ^ 2 - 2 M rCoeff[[1]] + a^2) / (rCoeff[[1]] 
        ^ 2 + a^2) ^ 2 IdentityMatrix[n]) . PsiPm[m, a, r, thetaList[[jSourceMax
         + 1]]];
      Do[
        row = lPosToCol[l, ijToPos[i, jSourceMax, iMax], n];
        vec[[row]] = vec[[row]] + data[[l]];
        ,
        {l, 1, n}
      ];
      \[Theta] = thetaList[[jSourceMin - 1]];
      data = (d1SecP1[\[CapitalDelta]\[Theta]] Bfunc[m, a, \[Omega], rCoeff, \[Theta]][[All, All, 1]] + 
        d2SecP1[\[CapitalDelta]\[Theta]] (rCoeff[[1]] ^ 2 - 2 M rCoeff[[1]] + a^2) / (rCoeff[[1]] 
        ^ 2 + a^2) ^ 2 IdentityMatrix[n]) . PsiPm[m, a, r, thetaList[[jSourceMin
        ]]];
      Do[
        row = lPosToCol[l, ijToPos[i, jSourceMin - 1, iMax], n];
        vec[[row]] = vec[[row]] - data[[l]];
        ,
        {l, 1, n}
      ];
      \[Theta] = thetaList[[jSourceMax + 1]];
      data = (d1SecM1[\[CapitalDelta]\[Theta]] Bfunc[m, a, \[Omega], rCoeff, \[Theta]][[All, All, 1]] + 
        d2SecM1[\[CapitalDelta]\[Theta]] (rCoeff[[1]] ^ 2 - 2 M rCoeff[[1]] + a^2) / (rCoeff[[1]] 
        ^ 2 + a^2) ^ 2 IdentityMatrix[n]) . PsiPm[m, a, r, thetaList[[jSourceMax
        ]]];
      Do[
        row = lPosToCol[l, ijToPos[i, jSourceMax + 1, iMax], n];
        vec[[row]] = vec[[row]] - data[[l]];
        ,
        {l, 1, n}
      ];
      ,
      {i, iSourceMin, iSourceMax}
    ];
    Do[
      \[Theta] = thetaList[[j]];
      rCoeff = {rSubrPlusList[[iSourceMin]] + rPlus};
      r = rSubrPlusList[[iSourceMin - 1]] + rPlus;
      data = (d1SecM1[\[CapitalDelta]rStar] Afunc[m, a, \[Omega], rCoeff, \[Theta]][[All, All, 1]]
         + d2SecM1[\[CapitalDelta]rStar] IdentityMatrix[n]) . PsiPm[m, a, r, \[Theta]];
      Do[
        row = lPosToCol[l, ijToPos[iSourceMin, j, iMax], n];
        vec[[row]] = vec[[row]] + data[[l]];
        ,
        {l, 1, n}
      ];
      rCoeff = {rSubrPlusList[[iSourceMax]] + rPlus};
      r = rSubrPlusList[[iSourceMax + 1]] + rPlus;
      data = (d1SecP1[\[CapitalDelta]rStar] Afunc[m, a, \[Omega], rCoeff, \[Theta]][[All, All, 1]]
         + d2SecP1[\[CapitalDelta]rStar] IdentityMatrix[n]) . PsiPm[m, a, r, \[Theta]];
      Do[
        row = lPosToCol[l, ijToPos[iSourceMax, j, iMax], n];
        vec[[row]] = vec[[row]] + data[[l]];
        ,
        {l, 1, n}
      ];
      rCoeff = {rSubrPlusList[[iSourceMin - 1]] + rPlus};
      r = rSubrPlusList[[iSourceMin]] + rPlus;
      data = (d1SecP1[\[CapitalDelta]rStar] Afunc[m, a, \[Omega], rCoeff, \[Theta]][[All, All, 1]]
         + d2SecP1[\[CapitalDelta]rStar] IdentityMatrix[n]) . PsiPm[m, a, r, \[Theta]];
      Do[
        row = lPosToCol[l, ijToPos[iSourceMin - 1, j, iMax], n];
        vec[[row]] = vec[[row]] - data[[l]];
        ,
        {l, 1, n}
      ];
      rCoeff = {rSubrPlusList[[iSourceMax + 1]] + rPlus};
      r = rSubrPlusList[[iSourceMax]] + rPlus;
      data = (d1SecM1[\[CapitalDelta]rStar] Afunc[m, a, \[Omega], rCoeff, \[Theta]][[All, All, 1]]
         + d2SecM1[\[CapitalDelta]rStar] IdentityMatrix[n]) . PsiPm[m, a, r, \[Theta]];
      Do[
        row = lPosToCol[l, ijToPos[iSourceMax + 1, j, iMax], n];
        vec[[row]] = vec[[row]] - data[[l]];
        ,
        {l, 1, n}
      ];
      ,
      {j, jSourceMin, jSourceMax}
    ];
    Return[vec];
  ]


testSourceVector[m_, a_, r0_, wtDiam_, thetaSourceSize_, \[CapitalDelta]rStar_, \[CapitalDelta]\[Theta]_,
   rStarHguess_, rStarIguess_] :=
  Module[{rStarList, rSubrPlusList, thetaList, iSourceMin, iSourceMax,
     jSourceMin, jSourceMax, vec, rStarH, rStarL, rStar0, rStarR, rStarI,
     iMax, jMax, source2D, Phi2D, rPlus, n},
    n = 10;
    {rStarH, rStarL, rStar0, rStarR, rStarI} = getrStarParams[a, r0, 
      wtDiam, rStarHguess, rStarIguess];
    rStarList = getrStarList[\[CapitalDelta]rStar, rStarH, rStarL, rStar0, rStarR, 
      rStarI];
    rSubrPlusList = getrSubrPlusList[a, rStarList];
    thetaList = getThetaList[\[CapitalDelta]\[Theta], thetaSourceSize];
    {iSourceMin, iSourceMax} = getIsourceBounds[rStarList, rStarH, rStarL,
       rStar0, rStarR, rStarI];
    {jSourceMin, jSourceMax} = getJsourceBounds[\[CapitalDelta]\[Theta], thetaSourceSize];
    vec = sourceVector[m, a, r0, rStarList, rSubrPlusList, thetaList,
       iSourceMin, iSourceMax, jSourceMin, jSourceMax];
    iMax = Length[rStarList];
    jMax = Length[thetaList];
    source2D = Table[vec[[lPosToCol[l, ijToPos[i, j, iMax], n]]], {l, 1, n}, {i,
       iSourceMin + 1, iSourceMax - 1}, {j, jSourceMin + 1, jSourceMax - 1}
      ];
    rPlus = 1 + Sqrt[1 - a^2];
    Phi2D = Table[PsiPm[m, a, rSubrPlusList[[i]] + rPlus, thetaList[[
      j]]][[l]], {l, 1, n}, {i, iSourceMin + 1, iSourceMax - 1}, {j, jSourceMin + 1, jSourceMax
       - 1}];
    Return[{source2D, Phi2D, rStarList, rSubrPlusList, thetaList, iSourceMin,
       iSourceMax, jSourceMin, jSourceMax}];
  ]


getFm[sol_, m_, rStarList_, rSubrPlusList_, thetaList_, iSourceMin_, 
  iSourceMax_, jSourceMin_, jSourceMax_] :=
  Module[{solSource, \[CapitalPsi], d\[CapitalPsi]drStar, d\[CapitalPsi]d\[Theta], interp, interpDrStar, interpD\[Theta],
     rStar, \[Theta], \[Omega], rPlus, iMax, jMax, \[CapitalDelta]rStar, \[CapitalDelta]\[Theta], rStar0, r0, rSubrPlus0, 
    d\[CapitalPhi]dr, d\[CapitalPhi]dt, d\[CapitalPhi]d\[Phi]},
    \[Omega] = m \[CapitalOmega];
    rPlus = 1 + Sqrt[1 - a^2];
    iMax = Length[rStarList];
    jMax = Length[thetaList];
    rStar0 = (rStarList[[(iSourceMax + iSourceMin - 1) / 2]] + rStarList[[
      (iSourceMax + iSourceMin + 1) / 2]]) / 2;
    rSubrPlus0 = (rSubrPlusList[[(iSourceMax + iSourceMin - 1) / 2]] 
      + rSubrPlusList[[(iSourceMax + iSourceMin + 1) / 2]]) / 2;
    r0 = rSubrPlus0 + rPlus;
    \[CapitalDelta]rStar = (rStarList[[-1]] - rStarList[[1]]) / (iMax - 1);
    \[CapitalDelta]\[Theta] = (thetaList[[-1]] - thetaList[[1]]) / (jMax - 1);
    solSource = Partition[sol, jMax];
    interp[rStar_, \[Theta]_] = Interpolation[Flatten[Table[{rStarList[[i]],
       thetaList[[j]], solSource[[i, j]]}, {i, (iSourceMax + iSourceMin - 1
      ) / 2 - 1, (iSourceMax + iSourceMin + 1) / 2 + 1}, {j, (jSourceMax + 
      jSourceMin - 1) / 2 - 1, (jSourceMax + jSourceMin + 1) / 2 + 1}], 1],
       InterpolationOrder -> 3][rStar, \[Theta]];
    interpDrStar[rStar_, \[Theta]_] = D[interp[rStar, \[Theta]], rStar];
    \[CapitalPsi] = interp[rStar0, \[Pi] / 2];
    d\[CapitalPsi]drStar = interpDrStar[rStar0, \[Pi] / 2];
    d\[CapitalPhi]dr =
      If[m == 0,
        Re[drStardr[a, rSubrPlus0] d\[CapitalPsi]drStar / r0 - \[CapitalPsi] / r0^2]
        ,
        2 Re[(drStardr[a, rSubrPlus0] d\[CapitalPsi]drStar / r0 + \[CapitalPsi] (I m d\[CapitalDelta]\[Phi]dr[a,
           rSubrPlus0] / r0 - 1 / r0^2)) Exp[I m \[CapitalDelta]\[Phi][a, rSubrPlus0]]]
      ];
    d\[CapitalPhi]dt =
      If[m == 0,
        0
        ,
        2 Re[-I \[Omega] \[CapitalPsi] Exp[I m \[CapitalDelta]\[Phi][a, rSubrPlus0]]] / r0
      ];
    d\[CapitalPhi]d\[Phi] =
      If[m == 0,
        0
        ,
        2 Re[I m \[CapitalPsi] Exp[I m \[CapitalDelta]\[Phi][a, rSubrPlus0]]] / r0
      ];
    {d\[CapitalPhi]dt, d\[CapitalPhi]dr, d\[CapitalPhi]d\[Phi]}
  ]


mRunField[m_, a_, r0_, wtDiam_, thetaSourceSize_, \[CapitalDelta]rStar_, \[CapitalDelta]\[Theta]_, rStarHguess_,
   rStarIguess_] :=
  Module[{sol, rStarList, rSubrPlusList, thetaList, iSourceMin, iSourceMax,
     jSourceMin, jSourceMax, iMax, jMax, mat, vec, rStarH, rStarL, rStar0,
     rStarR, rStarI, n, sol2D},
    n = 10;
    {rStarH, rStarL, rStar0, rStarR, rStarI} = getrStarParams[a, r0, 
      wtDiam, rStarHguess, rStarIguess];
    rStarList = getrStarList[\[CapitalDelta]rStar, rStarH, rStarL, rStar0, rStarR, 
      rStarI];
    rSubrPlusList = getrSubrPlusList[a, rStarList];
    thetaList = getThetaList[\[CapitalDelta]\[Theta], thetaSourceSize];
    {iSourceMin, iSourceMax} = getIsourceBounds[rStarList, rStarH, rStarL,
       rStar0, rStarR, rStarI];
    {jSourceMin, jSourceMax} = getJsourceBounds[\[CapitalDelta]\[Theta], thetaSourceSize];
      
    iMax = Length[rStarList];
    jMax = Length[thetaList];
    mat = couplingMatrixSec[m, a, r0, rStarList, rSubrPlusList, thetaList
      ];
    vec = sourceVectorPoint[m, a, r0, rStarList, rSubrPlusList, thetaList,
       iSourceMin, iSourceMax, jSourceMin, jSourceMax];
    sol = LinearSolve[mat, vec, Method -> "Pardiso"];
    sol2D = Table[sol[[lPosToCol[l, ijToPos[i, j, iMax], n]]], {l, 1,
       n}, {i, 1, iMax}, {j, 1, jMax}];
    Return[sol2D];
  ]


mRunFieldRet[m_, wtDiam_, thetaSourceSize_, \[CapitalDelta]rStar_, \[CapitalDelta]\[Theta]_, rStarHguess_, 
  rStarIguess_] :=
  Module[{sol, sing, rPlus, rStarList, rSubrPlusList, thetaList, iSourceMin, iSourceMax,
     jSourceMin, jSourceMax, iMax, jMax, mat, vec, rStarH, rStarL, rStar0,
     rStarR, rStarI},
    {rStarH, rStarL, rStar0, rStarR, rStarI} = getrStarParams[a, r0, 
      wtDiam, rStarHguess, rStarIguess];
    rStarList = getrStarList[\[CapitalDelta]rStar, rStarH, rStarL, rStar0, rStarR, 
      rStarI];
    rSubrPlusList = getrSubrPlusList[a, rStarList];
    thetaList = getThetaList[\[CapitalDelta]\[Theta], thetaSourceSize];
    {iSourceMin, iSourceMax} = getIsourceBounds[rStarList, rStarH, rStarL,
       rStar0, rStarR, rStarI];
    {jSourceMin, jSourceMax} = getJsourceBounds[\[CapitalDelta]\[Theta], thetaSourceSize];
      
    iMax = Length[rStarList];
    jMax = Length[thetaList];
    mat = couplingMatrixFast[m, rStarList, rSubrPlusList, thetaList];
      
    vec = sourceVector[m, rStarList, rSubrPlusList, thetaList, iSourceMin,
       iSourceMax, jSourceMin, jSourceMax];
    sol = LinearSolve[mat, vec, Method -> "Pardiso"];
    sol = Partition[sol, jMax];
    rPlus = 1+Sqrt[1-a^2];
    sing = Table[If[(iSourceMin<=i<=iSourceMax)&&(jSourceMin<=j<=jSourceMax),
    PsiPm[m, rSubrPlusList[[i]]+rPlus, thetaList[[j]]]Exp[-I m \[CapitalDelta]\[Phi][a, rSubrPlusList[[i]]]]/(2 \[Pi])
    ,0],{i,1,iMax},{j,1,jMax}];
    sol = Flatten[Table[{rStarList[[i]],thetaList[[j]],sol[[i,j]]+sing[[i,j]]},{i,1,iMax},{j,1,jMax}],1]
  ]


mRun[m_, wtDiam_, thetaSourceSize_, \[CapitalDelta]rStar_, \[CapitalDelta]\[Theta]_, rStarHguess_, rStarIguess_
  ] :=
  Module[{sol, rStarList, rSubrPlusList, thetaList, iSourceMin, iSourceMax,
     jSourceMin, jSourceMax, iMax, jMax, mat, vec, rStarH, rStarL, rStar0,
     rStarR, rStarI},
    {rStarH, rStarL, rStar0, rStarR, rStarI} = getrStarParams[a, r0, 
      wtDiam, rStarHguess, rStarIguess];
    rStarList = getrStarList[\[CapitalDelta]rStar, rStarH, rStarL, rStar0, rStarR, 
      rStarI];
    rSubrPlusList = getrSubrPlusList[a, rStarList];
    thetaList = getThetaList[\[CapitalDelta]\[Theta], thetaSourceSize];
    {iSourceMin, iSourceMax} = getIsourceBounds[rStarList, rStarH, rStarL,
       rStar0, rStarR, rStarI];
    {jSourceMin, jSourceMax} = getJsourceBounds[\[CapitalDelta]\[Theta], thetaSourceSize];
      
    iMax = Length[rStarList];
    jMax = Length[thetaList];
    mat = couplingMatrixFast[m, rStarList, rSubrPlusList, thetaList];
      
    vec = sourceVector[m, rStarList, rSubrPlusList, thetaList, iSourceMin,
       iSourceMax, jSourceMin, jSourceMax];
    sol = LinearSolve[mat, vec, Method -> "Pardiso"];
    getFm[sol, m, rStarList, rSubrPlusList, thetaList, iSourceMin, iSourceMax,
       jSourceMin, jSourceMax]
  ]


mRunFourth[m_, wtDiam_, thetaSourceSize_, \[CapitalDelta]rStar_, \[CapitalDelta]\[Theta]_, rStarHguess_, rStarIguess_
  ] :=
  Module[{sol, rStarList, rSubrPlusList, thetaList, iSourceMin, iSourceMax,
     jSourceMin, jSourceMax, iMax, jMax, mat, vec, rStarH, rStarL, rStar0,
     rStarR, rStarI},
    {rStarH, rStarL, rStar0, rStarR, rStarI} = getrStarParams[a, r0, 
      wtDiam, rStarHguess, rStarIguess];
    rStarList = getrStarList[\[CapitalDelta]rStar, rStarH, rStarL, rStar0, rStarR, 
      rStarI];
    rSubrPlusList = getrSubrPlusList[a, rStarList];
    thetaList = getThetaList[\[CapitalDelta]\[Theta], thetaSourceSize];
    {iSourceMin, iSourceMax} = getIsourceBounds[rStarList, rStarH, rStarL,
       rStar0, rStarR, rStarI];
    {jSourceMin, jSourceMax} = getJsourceBounds[\[CapitalDelta]\[Theta], thetaSourceSize];
      
    iMax = Length[rStarList];
    jMax = Length[thetaList];
    mat = couplingMatrixFourth[m, rStarList, rSubrPlusList, thetaList];
      
    vec = sourceVectorFourth[m, rStarList, rSubrPlusList, thetaList, iSourceMin,
       iSourceMax, jSourceMin, jSourceMax];
    sol = LinearSolve[mat, vec, Method -> "Pardiso"];
    getFm[sol, m, rStarList, rSubrPlusList, thetaList, iSourceMin, iSourceMax,
       jSourceMin, jSourceMax]
  ]


R2[n_, h1_, h2_, A1_, A2_] := (A2 h1^n - A1 h2^n)/(h1^n - h2^n)


R3[n_, h1_, h2_, h3_, A1_, A2_, A3_] := (A3*h1^n*(h1 - h2)*h2^n + (-(A2*h1^n*(h1 - h3)) + A1*h2^n*(h2 - h3))*h3^n)/
 (h2^n*(h2 - h3)*h3^n + h1^(1 + n)*(h2^n - h3^n) + h1^n*(-h2^(1 + n) + h3^(1 + n)))


R4[n_, h1_, h2_, h3_, h4_, A1_, A2_, A3_, A4_] := (A4*h1^n*(h1 - h2)*h2^n*(h1 - h3)*(h2 - h3)*h3^n + 
  (-(A3*h1^n*(h1 - h2)*h2^n*(h1 - h4)*(h2 - h4)) + 
    h3^n*(A2*h1^n*(h1 - h3)*(h1 - h4) - A1*h2^n*(h2 - h3)*(h2 - h4))*(h3 - h4))*
   h4^n)/(-(h2^n*(h2 - h3)*h3^n*(h2 - h4)*(h3 - h4)*h4^n) + 
  h1^(2 + n)*(h3^n*(h3 - h4)*h4^n + h2^(1 + n)*(h3^n - h4^n) + 
    h2^n*(-h3^(1 + n) + h4^(1 + n))) + 
  h1^(1 + n)*(h3^n*h4^n*(-h3^2 + h4^2) + h2^(2 + n)*(-h3^n + h4^n) + 
    h2^n*(h3^(2 + n) - h4^(2 + n))) + h1^n*(h3^(1 + n)*(h3 - h4)*h4^(1 + n) + 
    h2^(2 + n)*(h3^(1 + n) - h4^(1 + n)) + h2^(1 + n)*(-h3^(2 + n) + h4^(2 + n))))


resolutionConvergeFm[m_, rStarHguess_, rStarIguess_, wtDiam_, thetaSourceSize_,
   tol_, cumulF_] :=
  Module[{converge, converge2, converge3, \[CapitalDelta]rStarGuess, rStarH, rStarL,
     rStar0, rStarR, rStarI, j, \[CapitalDelta]rStar, \[CapitalDelta]\[Theta], Fm, test, newFm, oldFm},
    {rStarH, rStarL, rStar0, rStarR, rStarI} = getrStarParams[a, r0, 
      wtDiam, rStarHguess, rStarIguess];
    converge = {};
    \[CapitalDelta]rStarGuess = 0.7;
    j = Round[wtDiam / \[CapitalDelta]rStarGuess];
    If[EvenQ[j],
      j = j + 1
    ];
    \[CapitalDelta]rStar = wtDiam / j;
    \[CapitalDelta]\[Theta] = thetaSourceSize / (j);
    Fm = mRun[m, wtDiam, thetaSourceSize, \[CapitalDelta]rStar, \[CapitalDelta]\[Theta], rStarHguess, rStarIguess
      ];
    converge = Append[converge, {\[CapitalDelta]rStar, Fm}];
    \[CapitalDelta]rStarGuess = \[CapitalDelta]rStarGuess * 0.7;
    j = Round[wtDiam / \[CapitalDelta]rStarGuess];
    If[EvenQ[j],
      j = j + 1
    ];
    \[CapitalDelta]rStar = wtDiam / j;
    \[CapitalDelta]\[Theta] = thetaSourceSize / (j);
    Fm = mRun[m, wtDiam, thetaSourceSize, \[CapitalDelta]rStar, \[CapitalDelta]\[Theta], rStarHguess, rStarIguess
      ];
    converge = Append[converge, {\[CapitalDelta]rStar, Fm}];
    oldFm = Fm;
    newFm = R2[2, converge[[-2, 1]], converge[[-1, 1]], converge[[-2,
       2]], converge[[-1, 2]]];
    test = 0;
    While[
      test == 0 ||
        AnyTrue[
          Table[
            Abs[newFm[[j]] - oldFm[[j]]] >
              0.3 tol
                If[Max[Abs[cumulF[[j]]], Abs[newFm[[j]]]] < 10 ^ -10,
                  
                  10^10
                  ,
                  Max[Abs[cumulF[[j]]], Abs[newFm[[j]]]]
                ]
            ,
            {j, 2, 2}
          ]
          ,
          TrueQ
        ]
      ,
      test = 1;
      \[CapitalDelta]rStarGuess = \[CapitalDelta]rStarGuess * 0.7;
      j = Round[wtDiam / \[CapitalDelta]rStarGuess];
      If[EvenQ[j],
        j = j + 1
      ];
      \[CapitalDelta]rStar = wtDiam / j;
      \[CapitalDelta]\[Theta] = thetaSourceSize / (j);
      Fm = mRun[m, wtDiam, thetaSourceSize, \[CapitalDelta]rStar, \[CapitalDelta]\[Theta], rStarHguess, 
        rStarIguess];
      converge = Append[converge, {\[CapitalDelta]rStar, Fm}];
      oldFm = newFm;
      newFm =  R3[2, converge[[-3, 1]], converge[[-2, 1]], converge[[-1,
       1]], converge[[-3, 2]], converge[[-2, 2]], converge[[-1, 2]]];
      Print["\[CapitalDelta]r* = ", \[CapitalDelta]rStar, ", \[CapitalDelta]\[Theta] = ", \[CapitalDelta]\[Theta], ", Fm = ", NumberForm[newFm,
         10]];
      
    ];
    Return[newFm]
  ]


domainConvergeFm[m_, rStarHguess_, wtDiamGuess_, thetaSourceSize_, tol_,
   cumulF_] :=
  Module[{j, wtDiam, rStar0, converge, \[CapitalDelta]rStarIguess, \[CapitalDelta]rStarI, test, newFm,
     oldFm, rStarIguess, Fm},
    j = Ceiling[(2 \[Pi] / \[CapitalOmega]) / wtDiamGuess];
    wtDiam = (2 \[Pi] / \[CapitalOmega]) / j;
    rStar0 = getrStarFromrSubrPlus[a, r0 - (1 - Sqrt[1 - a^2])];
    rStarIguess = Round[80.0 / wtDiam] wtDiam + wtDiam / 2 + rStar0;
      
    converge = {};
    \[CapitalDelta]rStarIguess = 50.0;
    j = Ceiling[\[CapitalDelta]rStarIguess / (2 \[Pi] / \[CapitalOmega])];
    \[CapitalDelta]rStarI = j (2 \[Pi] / \[CapitalOmega]);
    Fm = resolutionConvergeFm[m, rStarHguess, rStarIguess, wtDiam, thetaSourceSize,
       tol, cumulF];
    converge = Append[converge, {1 / rStarIguess, Fm}];
    Print["r*I = ", rStarIguess, ", Fm = ", NumberForm[Fm, 10]];
    rStarIguess = rStarIguess + \[CapitalDelta]rStarI;
    Fm = resolutionConvergeFm[m, rStarHguess, rStarIguess, wtDiam, thetaSourceSize,
       tol, cumulF];
    converge = Append[converge, {1 / rStarIguess, Fm}];
    oldFm = Fm;
    newFm = R2[3, converge[[-2, 1]], converge[[-1, 1]], converge[[-2,
       2]], converge[[-1, 2]]];
    Print["r*I = ", rStarIguess, ", Fm = ", NumberForm[Fm, 10]];
    rStarIguess = rStarIguess + \[CapitalDelta]rStarI;
    test = 0;
    While[
      test == 0 ||
        AnyTrue[
          Table[
            Abs[newFm[[j]] - oldFm[[j]]] >
              tol
              If[Max[Abs[cumulF[[j]]], Abs[newFm[[j]]]] < 10 ^ -10,
                  10^10
                  ,
                  Max[Abs[cumulF[[j]]], Abs[newFm[[j]]]]
                ]
            ,
            {j, 2, 2}
          ]
          ,
          TrueQ
        ]
      ,
      test = 1;
      Fm = resolutionConvergeFm[m, rStarHguess, rStarIguess, wtDiam, 
        thetaSourceSize, tol, cumulF];
      converge = Append[converge, {1 / rStarIguess, Fm}];
      oldFm = newFm;
      newFm = 
    newFm = R3[3, converge[[-3, 1]], converge[[-2, 1]], converge[[-1,
        1]], converge[[-3, 2]], converge[[-2, 2]], converge[[-1, 2]]];
      Print["r*I = ", rStarIguess, ", Fm = ", NumberForm[newFm, 10]];
        
      rStarIguess = rStarIguess + \[CapitalDelta]rStarI;
      
    ];
    Return[newFm]
  ]


getF[rStarHguess_, wtDiamGuess_, thetaSourceSize_, tol_] :=
  Module[{m, cumulF, FmList, Fm},
    m = 0;
    Print["m = ", m];
    Fm = domainConvergeFm[m, rStarHguess, wtDiamGuess, thetaSourceSize,
       0.1 tol, {0, 0, 0}];
    FmList = {Fm};
    cumulF = Fm;
    m = 1;
    Print["m = ", m];
    Fm = domainConvergeFm[m, rStarHguess, wtDiamGuess, thetaSourceSize,
       0.3 tol, {0, 0, 0}];
    AppendTo[FmList, Fm];
    cumulF = cumulF + Fm;
    m = 2;
    Print["m = ", m];
    Fm = domainConvergeFm[m, rStarHguess, wtDiamGuess, thetaSourceSize,
       1.0 tol, {0, 0, 0}];
    AppendTo[FmList, Fm];
    cumulF = cumulF + Fm;
    Do[
      Print["m = ", m];
      Fm = domainConvergeFm[m, rStarHguess, wtDiamGuess, thetaSourceSize,
         3.0 tol, cumulF];
      AppendTo[FmList, Fm];
      cumulF = cumulF + Fm;
      ,
      {m, 3, 20}];
    Return[FmList]
  ]; 
