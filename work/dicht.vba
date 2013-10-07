Function Dicht(Temperatur)
' Dichte des Wassers, Paul, 1985
    Dim a(5) As Double
    Dim D As Double
    a(0) = 999.84102229
    a(1) = 0.067477741278
    a(2) = -0.0089764830531
    a(3) = 0.000088534831535
    a(4) = -6.5515551052E-07
    D = a(4)
    For j = 3 To 0 Step -1
        D = a(j) + D * Temperatur
    Next j
    Dicht = D
End Function
