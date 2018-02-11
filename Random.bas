Attribute VB_Name = "Random"
' Keith Chamberlain
' www.ChamberlainStatistics.com
' Released under the GNU v. 3
' Code for Blog post on 28-Dec-17:
' The final part in a 5 part series on generating
'   random (seeded) data in R, Python and Excel.
' "Surrogate calibration data in R, Python and Excel -
'   the final take: Coding in the VBA interface"
' Uploaded to repo on 10-Feb-18

Option Explicit

' Write to Cell Range
Private Sub writeCellRange(arr As Variant, _
                           myRange As Range, _
                           header As String)
    myRange.Select
    myRange.Value = _
            WorksheetFunction.Transpose(arr)
    myRange.Item(0, 1).Value = header
    
End Sub

' Read Data from worksheet cells
Private Function readCellRange(myRange As Range) _
                 As Variant
    myRange.Select
    readCellRange = WorksheetFunction.Transpose(myRange)
End Function

Private Sub main()
    Dim data As Variant, Y As Range, X As Range, _
        ydata As Variant, xdata As Variant, _
        y2data As Variant, x2data As Variant, _
        YOut As Range, XOut As Range, XOut2 As Range, _
        XOut3 As Range, xSqr() As Variant, _
        xCube() As Variant, i As Long
    ' Set input and output Ranges. Alternatively,
    ' could set one range and just offset it.
    Set Y = Excel.Range("A2:A7") ' Y is to read from
    Set X = Excel.Range("B2:B7") ' X is to read from
    Set YOut = Excel.Range("C2:C7") ' YOut is to write
    Set XOut = Excel.Range("D2:D7") ' XOut is to write
    Set XOut2 = Excel.Range("E2:E7") ' XOut2 is to write
    Set XOut3 = Excel.Range("F2:F7") ' XOut3 is to write
    
    ' Read data and grab normed values
    ydata = readCellRange(Y)
    xdata = readCellRange(X)
    y2data = rNorm(25, 10, ydata, 1, True)
    x2data = rNorm(0, 10, xdata, 2, True)
    ReDim xSqr(1 To UBound(x2data))
    ReDim xCube(1 To UBound(x2data))
    ' Calculate ^2 and ^3 values. 1 cell at once? Ich.
    For i = 1 To UBound(x2data)
        xSqr(i) = x2data(i) ^ 2
        xCube(i) = x2data(i) ^ 3
    Next i
    ' Write data
    Call writeCellRange(y2data, YOut, "YRnd")
    Call writeCellRange(x2data, XOut, "XRnd")
    Call writeCellRange(xSqr, XOut2, "XRndSqr")
    Call writeCellRange(xCube, XOut3, "XRndCube")
End Sub

Public Function rNorm(seed As Integer, _
                      randomizer As Integer, _
                      data As Variant, sd As Variant, _
                      aboutLine As Boolean) _
                      As Variant
    Dim i As Integer, uniform As Variant, _
        norm As Variant, data2 As Variant, _
        mean As Variant
    mean = WorksheetFunction.Average(data)
    uniform = randomWithSeed(seed, UBound(data), _
              randomizer)
    norm = normed(uniform, mean, sd)
    If (aboutLine = True) Then
        For i = 1 To (UBound(data))
            data(i) = norm(i) * data(i)
        Next i
        rNorm = data
    Else
        rNorm = norm
    End If
End Function
      
' Wrapper for Rnd() that also calls setSeed().
' Works for Variant arrays.
Public Function randomWithSeed(seed As Integer, _
                               length As Integer, _
                               randomizer As Integer) _
                               As Variant
    Dim i As Integer
    Dim X As Variant
    ReDim X(0 To length)
    
    setSeed (seed)
    For i = 0 To length
        X(i) = rndSeeded(randomizer)
    Next i
    randomWithSeed = X
End Function

' Set seed similar to R example, as in:
' https://support.office.com/en-us/article/Rnd-Function-503cd2e4-3949-413f-980a-ed8fb35c1d80?CorrelationId=1f8bb1af-523f-403d-87f1-89d10b81bbf5&ui=en-US&rs=en-US&ad=US&ocmsassetID=HA001228901
Public Function setSeed(seed As Integer) As Variant
    setSeed = Rnd(-1 * Abs(seed))
End Function

' Wrapper for Rnd(). Need to call Randomize each time
' with the same randomizer. setSeed() only needs
' to be called once.
Public Function rndSeeded(randomizer As Integer) _
                As Variant
    Randomize randomizer
    rndSeeded = Rnd
End Function

' Wrapper for Rnd()
Public Function randomUnSeeded() As Variant
    randomSeeded = Rnd()
End Function

' Wrapper for WorksheetFunction.Norm_Inv()
Public Function normed(rand As Variant, _
                mean As Variant, _
                sd As Variant) _
                As Variant
    Dim i As Integer, data As Variant
    ReDim data(0 To UBound(rand))
    For i = 0 To UBound(rand)
        data(i) = WorksheetFunction.Norm_Inv(rand(i), _
                 mean, sd)
    Next i
    normed = data
End Function

' Custom function
Public Function intToNorm(normValue As Variant, _
                i_nteger As Integer) _
                As Variant
    intToNorm = i_nteger * normValue
End Function

