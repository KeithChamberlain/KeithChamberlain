Attribute VB_Name = "massDiff"
' Keith Chamberlain
' www.ChamberlainStatistics.com
' This code is released under GNU 3.0 and may be used for any purpose.
' Document revision 2.0 to add refDiff() [before commit] 10Feb18

Option Explicit

Private Sub main()
End Sub

' Function to return the PPM mass difference between the observed and theoretical masses
' after conditioning the input for diff() (a wrapper to diff()) to be specific for masses
Public Function massDiff(Optional observed_Mass As Double = 1E-16, _
                         Optional exact_Mass As Double = 1E-16, _
                         Optional fitOption As Long = 2, _
                         Optional reference_Mass As Double = 200#) As Variant
    ' Error handling
    If observed_Mass <= 0 Then observed_Mass = 1E-16
    If exact_Mass <= 0 Then exact_Mass = 1E-16
    If reference_Mass <= 0 Then reference_Mass = 1E-16
    If fitOption <= 2 Then fitOption = 2
    If fitOption >= 5 Then fitOption = 5
    ' Calculator call
    massDiff = diff(observed_Mass, exact_Mass, fitOption, reference_Mass)
    If (massDiff = "Option Out of Range") = False Then massDiff = massDiff * 1000000
    If massDiff = -0 Then massDiff = 0 ' To clean up the -0 condition from 0/(-1).
End Function

' Function to return the classical percent difference between the observed and expected
' values after conditioning the input for diff() (a wrapper to diff()) for option 1 of
' diff() only, that also handles negative values.
Public Function classicPercentDiff(Optional value1 As Double = 1E-16, _
                               Optional value2 As Double = 1E-16) As Variant
    ' Error handling
    If value1 = 0 Then value1 = 1E-16
    If value2 = 0 Then value2 = 1E-16
    ' Calculator call
    classicPercentDiff = diff(value1, value2, 1) * 100
    If classicPercentDiff = -0 Then classicPercentDiff = 0 ' To clean up the -0 condition
                                                           ' from 0/(-1).
End Function

' The lower level and more general difference function. Minimal error checking.
Private Function diff(Optional observed As Double = 1E-16, _
                     Optional expected As Double = 1E-16, _
                     Optional fitOption As Long = 2, _
                     Optional reference As Double = 1E-16) As Variant
    ' Declarations
    Dim setit As Long
    setit = 1
    ' Case
    Select Case fitOption
        Case 1 ' Result * 100 = Classic % difference (e.g. normalized by the mean)
            If (WorksheetFunction.Average(observed, expected) <> 0) Then
                diff = (observed - expected) / _
                        WorksheetFunction.Average(observed, expected)
            Else: diff = (observed - expected) / 1E-16
            End If
        Case 2 ' Classic "mass" difference (e.g. normalized by expected)
            diff = (observed - expected) / expected
        ' Log difference
        Case 3 ' Log PPM difference, similar to classic
            diff = (Log(observed) - Log(expected))
        Case 4 ' Chamberlain difference
            diff = ((observed - expected) / reference)
        Case 5 ' Log Chamberlain PPM difference
            If (observed - expected) < 0 Then setit = -1
            diff = setit * (Log(reference + Abs(observed - expected)) - Log(reference))
        Case Else
            diff = "Option Out of Range" ' = CVErr(xlErrNA) ' Using a more sensible error
                                         ' instead. Requires that the function is a
                                         ' Variant to return strings AND doubles.
    End Select
End Function

' refDiff() provides a way to back-calculate what the reference
' would need to be to result in a given target-difference,
' using one method for the calculation.
Public Function refDiff(Optional difference As Double = 1E-16, _
                    Optional target As Double = 0.000001) _
                    As Variant
    refDiff = target / difference
End Function