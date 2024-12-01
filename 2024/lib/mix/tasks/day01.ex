defmodule Mix.Tasks.D01 do
  use Mix.Task

  import Puzzles.Day01

  @shortdoc "Day 01 puzzle runner"
  def run(args) do
    exampleInput = """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

    realInput = """
    85215   94333
    24582   34558
    98037   94333
    75786   66247
    45656   85863
    70998   87003
    30367   62007
    81780   23161
    90260   65786
    24710   86514
    14018   34310
    43565   47888
    59781   79173
    47761   71538
    85892   22181
    25701   61839
    18264   33438
    33747   43258
    39697   94333
    61838   37358
    70437   22496
    23562   26799
    11216   34419
    63191   11393
    88615   31544
    93481   62720
    29534   40919
    29935   18758
    95190   87857
    51306   33515
    30938   29652
    77253   30646
    66807   67041
    75203   67041
    15696   61800
    73541   29496
    52063   51002
    55826   40919
    79183   35633
    77348   44025
    65423   33750
    65816   10624
    52110   89611
    18201   22051
    27748   66807
    92259   42784
    14988   33500
    58623   64359
    88260   90432
    62079   77685
    45698   40919
    91705   64359
    53661   73356
    93541   28465
    76689   96938
    60498   93056
    25784   65786
    32811   24732
    62264   72520
    41995   43258
    38545   84426
    19555   72520
    73703   78074
    86068   72520
    25850   87009
    49433   10684
    74291   61839
    40711   10516
    37728   82526
    80842   40919
    24677   23062
    29575   52605
    20692   77673
    82910   68845
    33830   62419
    68434   73604
    62401   84426
    82646   70854
    38398   56339
    79408   51171
    63734   27770
    13556   24577
    75123   37015
    61363   69734
    95609   86251
    88545   27770
    50893   74026
    14882   87009
    91379   33438
    60224   52605
    72679   47888
    75505   59781
    10186   62720
    88379   64359
    90496   67041
    73586   93056
    95670   63191
    87141   59781
    13919   74934
    98555   74846
    87897   32687
    21880   62687
    29658   50213
    96248   83521
    15492   86906
    29496   53112
    25334   27594
    20643   86251
    56611   39477
    29416   24577
    74832   63164
    43193   43258
    81875   75725
    61800   61927
    24065   94333
    83853   31467
    17180   10130
    43720   37354
    65647   45425
    51476   10461
    57574   93056
    31072   83739
    75113   19490
    79797   26799
    72214   11294
    13116   48228
    93787   33438
    46134   19254
    40985   28931
    25988   11393
    23062   59151
    79383   11393
    75339   89198
    79708   29496
    13058   61839
    16278   45698
    80414   14142
    16351   40919
    54813   43258
    81250   87434
    50921   61839
    53645   43258
    70432   22523
    81070   32306
    38985   31359
    19798   99501
    92509   43258
    25063   66000
    44665   33438
    64806   93056
    74946   26300
    48029   30646
    77560   83292
    82468   95064
    29223   71914
    87584   63191
    93056   26799
    55648   29949
    14352   48844
    49873   34111
    56227   45698
    67703   63191
    68135   30646
    44722   90812
    65476   78955
    82704   47888
    50424   23079
    51796   70111
    85079   22013
    61910   66098
    84132   89544
    47927   62720
    61766   78856
    38484   42757
    57255   93056
    48855   78836
    73127   78836
    21236   61839
    15138   94333
    26829   65786
    48823   93140
    28401   63191
    75082   42757
    41215   26799
    67176   19254
    13673   78836
    53842   26386
    41964   59781
    46737   21097
    52719   29652
    20212   76559
    89201   82291
    98568   63191
    55425   93322
    69878   63283
    36501   27429
    16724   47888
    61592   81085
    41114   93056
    44198   47888
    47061   86251
    45289   56992
    47597   15576
    18834   66807
    31018   40919
    40775   78266
    25185   76021
    48462   64359
    26799   32811
    37199   19555
    94411   77560
    45846   48696
    16862   77508
    77988   94333
    89121   66807
    41240   94333
    88088   59151
    52125   29652
    12932   13054
    61211   28578
    26024   87009
    26312   55718
    14585   45698
    65939   91730
    68138   59781
    70317   78836
    83217   78836
    22036   26799
    63215   66807
    54110   21264
    12981   33069
    61857   29652
    54637   94333
    15241   93056
    19897   33438
    77229   86251
    55685   37316
    30161   66807
    61780   14101
    19289   17661
    48121   51324
    99355   72435
    62767   32811
    98670   19490
    85332   31012
    92588   18461
    67574   94379
    90643   66607
    95487   66807
    33530   46429
    87009   23062
    61978   27770
    53891   19254
    64426   58297
    70651   61800
    40653   40474
    45888   33438
    55241   44527
    11126   42398
    92609   93056
    21760   22129
    72079   51542
    41177   26196
    67610   68036
    62720   45698
    38295   64359
    69482   93056
    24219   66807
    69511   45698
    30242   24577
    46626   18573
    36316   80909
    40214   72520
    16785   10921
    27770   11393
    70571   43258
    50406   30875
    76411   98707
    75186   77560
    80536   11587
    74209   78836
    20188   72520
    30587   57435
    44135   58144
    29251   46185
    73362   45698
    52443   53860
    61094   46357
    12167   47231
    51846   62720
    75168   23761
    97509   65786
    29546   42330
    83531   35185
    67737   19555
    85592   61105
    11431   29868
    38480   45698
    42360   17615
    14648   34009
    32741   52605
    97324   51113
    79881   77560
    71771   32811
    18334   52605
    68130   61839
    61546   47888
    14960   52980
    18988   99079
    30289   23062
    75930   29652
    21257   44357
    36448   47888
    86654   93056
    11876   43258
    66255   40252
    92074   36869
    29648   32811
    17522   72850
    52479   93056
    95416   73490
    69033   62720
    60062   92471
    67751   61800
    32285   41674
    14164   61800
    84112   78836
    28620   16338
    93425   19490
    25079   43258
    63507   30646
    25390   43258
    32858   23009
    95361   16283
    94333   74724
    40395   72462
    43206   62720
    92330   29652
    84078   24447
    72161   64359
    63204   75725
    16143   33438
    71756   69784
    27041   11228
    84087   94333
    17409   59151
    51792   14329
    52746   17484
    74015   23504
    59054   67041
    29297   19555
    84657   86854
    32609   23062
    17060   44956
    25874   78836
    58302   54942
    80865   36577
    26487   84426
    66212   63915
    15758   94333
    89851   47888
    76509   18449
    82093   86251
    59990   86771
    10461   26799
    63653   10461
    61308   26799
    89327   86251
    14989   22847
    17367   33851
    86991   51588
    41601   19490
    44063   92877
    65372   61839
    64013   61800
    53989   52845
    87917   19555
    54951   77327
    49657   40919
    37242   69223
    10122   67041
    20118   86251
    94367   70551
    23209   24123
    55363   19555
    65064   52605
    12293   61429
    90625   71173
    75864   59052
    75980   74726
    82806   77560
    50840   17142
    22139   64359
    56146   10907
    68559   63191
    15717   32811
    18446   64359
    32393   85038
    38555   78836
    92877   27770
    52476   51588
    22255   79119
    57023   75570
    89712   52605
    47141   10226
    98051   77560
    69509   53053
    51468   15072
    83074   61800
    78836   34010
    32010   77560
    82768   59151
    80962   17192
    37056   61800
    55438   60664
    24160   67041
    79310   59781
    36425   63941
    41091   19348
    47905   12402
    41536   44084
    29509   27770
    54028   18938
    20625   28465
    71827   20910
    47338   26799
    10967   33904
    75725   27770
    95800   59781
    64759   75725
    37476   63745
    78412   65786
    28130   33021
    83725   99593
    53024   29496
    45967   45698
    56671   26688
    41707   55835
    77665   64081
    48546   27175
    16309   17752
    16089   81097
    76903   57741
    31304   65786
    45381   47888
    61640   21302
    55671   38075
    91121   39769
    48837   84153
    68378   92214
    77462   59495
    36188   31985
    60269   25406
    78335   19555
    74685   98007
    16866   98832
    72995   52118
    26388   66807
    36575   45698
    46928   70337
    65265   88675
    84605   93056
    42658   49345
    56818   78302
    51576   61800
    95552   52605
    84846   84426
    52027   32811
    28477   19490
    64359   81028
    47203   15754
    52605   20196
    95437   70612
    72068   51272
    59641   67041
    24867   29652
    32292   21462
    67584   23062
    35182   57668
    19538   30815
    32007   12078
    92975   32366
    30032   66807
    41113   19555
    73576   24441
    26722   61839
    54882   40919
    99164   94333
    24703   55316
    68050   35855
    60454   22975
    27190   47888
    49592   33438
    56765   94333
    49584   40919
    40545   51588
    13088   65786
    92702   19555
    25522   98561
    70502   49926
    92893   40919
    11623   94333
    84426   72520
    12731   67041
    92300   66281
    21625   93056
    26632   86251
    28475   77846
    75712   50243
    75879   65786
    69842   32598
    95243   90308
    44031   63191
    63456   62720
    47057   90323
    32649   80072
    30244   85267
    18656   11393
    70657   89691
    59827   54749
    21580   17319
    34612   19555
    71392   19254
    66944   33438
    75302   38740
    35677   61839
    66839   28758
    43839   86251
    33590   11393
    70339   43258
    73961   84251
    39874   75536
    83280   13458
    76702   23062
    38544   19490
    50441   53429
    42757   61839
    67041   78836
    12497   25094
    42768   65593
    12205   19555
    95448   94430
    32384   40613
    21279   78836
    17428   65786
    10614   62720
    61245   59781
    17235   93810
    16999   31069
    63526   86251
    39490   65786
    95242   25169
    66925   87755
    22315   55633
    56338   64359
    20318   17296
    87536   71404
    76054   14998
    51588   61800
    13119   27770
    34345   10279
    20023   81145
    37131   19490
    42337   47888
    89951   61839
    79435   65253
    13465   45698
    29793   92877
    27722   15909
    86832   11477
    81965   78836
    20918   11393
    75215   96512
    85783   19555
    90525   59151
    15127   56714
    76419   66807
    45429   63191
    40503   51588
    77472   66807
    24459   64359
    39273   52605
    65912   29652
    41919   27969
    38024   62720
    79322   87432
    43054   33081
    23682   19254
    12316   67030
    34222   45698
    98966   78166
    34564   64359
    32039   19295
    39662   45698
    77296   62861
    11982   65786
    85184   27770
    23308   48241
    87694   79043
    65053   65212
    95042   94333
    63925   27924
    61794   47888
    46414   12815
    74588   34197
    29733   59781
    32203   95057
    82698   70210
    16989   92877
    79186   51588
    47914   52605
    49915   10461
    18124   37893
    12186   68323
    80439   26799
    91196   78836
    65806   66807
    64937   77371
    39013   97072
    35793   78836
    86223   67041
    68919   86251
    16242   61839
    26757   69116
    62000   65786
    91400   30878
    43258   66741
    74293   19490
    36472   17183
    72537   29652
    79963   73712
    89591   42757
    46045   59313
    55267   83489
    11274   29652
    46293   59781
    13186   63191
    31736   47888
    17707   51784
    11755   96140
    43161   51588
    16657   19490
    13779   86544
    77770   78265
    43502   61800
    26568   53664
    16788   47888
    92875   91405
    99320   66415
    19490   78836
    38003   28554
    12296   77560
    98989   40919
    90146   21633
    11563   97245
    52578   66743
    65786   44135
    68704   19490
    69963   19555
    16787   45698
    38189   14942
    79734   63191
    40154   35300
    57993   51909
    56091   64832
    28153   63191
    88207   15883
    57286   63191
    72520   26705
    50122   62720
    95885   14865
    25183   75725
    28985   52605
    81747   84420
    50078   35018
    76112   98498
    27757   75725
    56391   87304
    58234   61800
    90999   19597
    25733   19254
    65607   19490
    62203   99203
    59085   19490
    20052   72751
    28344   19008
    58344   93056
    34019   33438
    24970   43258
    72586   14704
    12892   11393
    67070   19555
    38881   43258
    16457   17416
    15116   59120
    91678   75725
    80586   19555
    33881   42757
    61688   71092
    69315   56598
    53321   93224
    59151   28716
    87226   19254
    73486   94333
    27149   79695
    44397   63191
    86370   66807
    28465   70737
    67420   94333
    97544   33438
    11326   32811
    65808   52518
    51991   51637
    38955   38228
    40299   86251
    98792   59151
    98158   27770
    15911   64175
    33107   29652
    91713   47922
    53452   81046
    31259   30646
    61299   62720
    68294   65343
    30551   93056
    35457   66807
    73631   11393
    30456   28781
    30406   27172
    61078   10461
    72853   63191
    11693   23540
    56942   43258
    36376   11728
    45741   65210
    37009   63191
    87300   23062
    61087   75044
    47331   46996
    47888   86249
    91532   29496
    70212   65786
    24577   94333
    26505   67442
    93930   20914
    21051   94088
    56106   33438
    47059   52605
    39701   27851
    76753   98828
    80218   65786
    15008   60952
    20406   30646
    19425   61800
    41697   59781
    77797   85385
    86369   60589
    15816   12411
    12414   75725
    91988   58454
    15219   43258
    92982   45698
    71767   21358
    78447   30646
    42788   19254
    19254   56308
    54247   27770
    64860   48561
    25737   75409
    13014   51764
    68315   75543
    89552   51588
    92384   77560
    12292   94277
    55549   54939
    90489   26402
    23051   47888
    87275   77315
    90750   49201
    19383   75055
    26480   52605
    11456   90963
    24861   66890
    45105   51719
    88412   93056
    41192   51588
    10159   24577
    22941   19555
    47473   93056
    38267   93056
    24304   10531
    86270   44766
    91359   33763
    35745   19254
    35994   32620
    35989   59781
    24968   73146
    33438   78836
    84317   59781
    16694   84697
    50035   43258
    46849   36981
    21158   10033
    38659   59151
    66814   99698
    62569   29496
    26374   85324
    17282   10461
    58720   69625
    67926   19490
    32437   47132
    87668   25151
    39798   36163
    65779   63191
    36260   41236
    80381   34952
    90225   94977
    66679   47888
    88157   47888
    13264   26799
    64409   51715
    93067   19254
    30142   52910
    42595   27572
    85722   93056
    30646   99178
    53833   17578
    38303   64359
    44152   61839
    91913   29652
    73350   31975
    99275   71900
    15131   25667
    61889   45155
    18935   66807
    41834   29496
    18515   26799
    31759   87780
    40919   64359
    20301   66807
    32312   51588
    73708   60185
    95227   93335
    92281   83328
    30610   76311
    74684   58155
    59712   72520
    65683   13851
    30738   36443
    31859   34438
    82346   86251
    46263   65786
    13554   19555
    67042   52605
    61839   29496
    75364   19490
    58929   59151
    66416   31691
    88263   79906
    78356   61800
    57336   19486
    53785   72520
    16902   45698
    61032   38568
    48258   19254
    73802   43258
    94584   74739
    40440   30646
    30176   66807
    70136   19254
    25421   64359
    57645   66829
    16176   33438
    20419   96424
    51694   66807
    42302   61839
    68569   45698
    93917   77560
    69691   20005
    53629   91608
    57088   67261
    71814   67923
    93253   86251
    17098   42479
    47293   19490
    81492   33438
    12600   53176
    22912   20117
    76064   42701
    44694   57113
    96409   19490
    61518   29936
    41719   56821
    80558   63191
    29918   61800
    51778   86034
    21399   64587
    84781   95919
    13131   53727
    17389   59781
    45549   97315
    85797   11393
    87747   96380
    32160   44227
    17589   63044
    15753   67638
    75220   40577
    40166   43258
    94668   65786
    59922   92500
    10582   52605
    79962   67041
    86178   45698
    74569   61475
    94528   38255
    59272   59781
    60494   81215
    60692   68282
    37369   71278
    26980   83758
    51039   35517
    75162   63191
    81331   65786
    29652   27770
    98663   19490
    43026   60359
    14599   58212
    13011   27770
    50257   94333
    13018   47888
    40970   29652
    46679   71924
    32462   75725
    81209   37345
    64600   43258
    96685   92198
    14635   24577
    66344   23062
    93459   11393
    49013   11393
    15256   97630
    52161   47888
    66454   19490
    78139   47888
    73936   19254
    43733   19555
    35548   94333
    11626   85812
    11393   15495
    58006   52605
    92271   56010
    31512   98555
    11545   62220
    66798   22932
    13234   29652
    70506   72520
    36041   79205
    78007   10461
    59850   67041
    77378   67041
    34128   94333
    31147   33438
    45913   33438
    97727   61839
    25073   25778
    86251   43258
    58420   55502
    43354   78836
    68038   86251
    67290   33438
    90859   59151
    67710   66807
    48934   16778
    22667   86065
    72666   22924
    77601   64359
    20458   10777
    99233   89451
    54526   13900
    79270   75407
    51276   23062
    54733   98555
    60594   40919
    61152   92938
    74123   42757
    13243   19555
    55324   20819
    55894   23062
    16368   34399
    16600   74703
    59353   72520
    19565   11393
    71782   87009
    31511   86251
    83349   96101
    23853   40439
    53938   19555
    18254   32811
    86817   33438
    78957   74568
    """

    input =
      cond do
        Enum.member?(args, "example") -> exampleInput
        true -> realInput
      end

    if Enum.member?(args, "part1"),
      do:
        input
        |> part1()
        |> IO.inspect(label: "Part 1 Results")

    if Enum.member?(args, "part2"),
      do:
        input
        |> part2()
        |> IO.inspect(label: "Part 2 Results")
  end
end
