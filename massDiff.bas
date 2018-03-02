Attribute VB_Name = "Module1"
' Mass functions provided by Keith Chamberlain
' www.ChamberlainStatistics.com
' Licensed for free and open use
' Doc Rev 2.0 10Feb18
' Rev 2.1 Added invertDiff() & cleaned up
'   line breaks. 27Feb18

Option Explicit

Private Sub main()
    Dim result As Variant
    result = invertDiff(1.007276466879, _
             1.007276466879 + 0.00054857990924, _
             548.5799, "ppm")
    MsgBox (result)
End Sub

' Function to return the PPM mass difference between
' the observed and theoretical masses after conditioning
' the input for diff() (a wrapper to diff()) to be
' specific for masses.
Public Function massDiff( _
        Optional observed_Mass As Double = 1E-16, _
        Optional exact_Mass As Double = 1E-16, _
        Optional fitOption As Long = 2, _
        Optional reference_Mass As Double = 200#) _
        As Variant
    ' Error handling
    If observed_Mass <= 0 Then observed_Mass = 1E-16
    If exact_Mass <= 0 Then exact_Mass = 1E-16
    If reference_Mass <= 0 Then reference_Mass = 1E-16
    If fitOption <= 2 Then fitOption = 2
    If fitOption >= 5 Then fitOption = 5
    ' Calculator call
    massDiff = diff(observed_Mass, exact_Mass, _
        fitOption, reference_Mass)
    If (massDiff = "Option Out of Range") = False Then
        massDiff = massDiff * 1000000#
    End If
    If massDiff = -0 Then
        massDiff = 0 ' To clean up the -0 condition
                     ' from 0/(-1).
    End If
End Function

' Function to return the classical percent difference
' between the observed and expected values after
' conditioning the input for diff() (a wrapper to diff())
' for option 1 of diff() only. Handles negative values.
Public Function classicPercentDiff( _
        Optional value1 As Double = 1E-16, _
        Optional value2 As Double = 1E-16) _
        As Variant
    ' Error handling
    If value1 = 0 Then value1 = 1E-16
    If value2 = 0 Then value2 = 1E-16
    ' Calculator call
    classicPercentDiff = diff(value1, value2, 1) * 100
    ' To clean up the -0 condition from 0/(-1).
    If classicPercentDiff = -0 Then classicPercentDiff = 0
End Function

' The lower level and more general difference function.
' Minimal error checking.
Private Function diff( _
        Optional observed As Double = 1E-16, _
        Optional expected As Double = 1E-16, _
        Optional fitOption As Long = 2, _
        Optional reference As Double = 1E-16) _
        As Variant
    ' Declarations
    Dim setit As Long
    setit = 1
    ' Case
    Select Case fitOption
        Case 1 ' Result * 100 = Classic % difference
               ' (e.g. normalized by the mean)
            If (WorksheetFunction.Average(observed, _
                expected) <> 0) Then
                diff = (observed - expected) / _
                        WorksheetFunction.Average(observed, _
                        expected)
            Else: diff = (observed - expected) / 1E-16
            End If
        Case 2 ' Classic "mass" difference (e.g.
               ' normalized by expected)
            diff = (observed - expected) / expected
        ' Log difference
        Case 3 ' Log PPM difference, similar to classic
            diff = (Log(observed) - Log(expected))
        Case 4 ' Chamberlain difference
            diff = ((observed - expected) / reference)
        Case 5 ' Log Chamberlain PPM difference
            If (observed - expected) < 0 Then setit = -1
            diff = setit * (Log(reference + Abs(observed - _
                    expected)) - Log(reference))
        Case Else
            ' = CVErr(xlErrNA) ' Using a more sensible error
            ' instead. Requires that the function is a
            ' Variant to return strings AND doubles.
            diff = "Option Out of Range"
    End Select
End Function

' invertDiff() provides a way to back-calculate what the
' reference would need to be to result in a given
' target-difference. Zeros are treated as near zeros.
' Negative values are permitted.
Public Function invertDiff( _
        Optional observed As Double = 1E-16, _
        Optional expected As Double = 1E-16, _
        Optional difference As Double = 1E-16, _
        Optional scale_ As String = "PPM") _
        As Variant
    ' Input checks. Avoiding zeros
    If (observed = 0) Then observed = 1E-16
    If (expected = 0) Then expected = 1E-16
    If (difference = 0) Then difference = 1E-16
    scale_ = UCase(scale_)
    ' Set some new variables for calculated values
    Dim newDiff As Double, scaledNewDiff As Double
    newDiff = (observed - expected)
    scaledNewDiff = 1E-16
    If (scale_ = "PPM") Then
        scaledNewDiff = newDiff * 1000000#
    ElseIf (scale_ = "PPT") Then
        scaledNewDiff = newDiff * 1000000000#
    ElseIf (scale_ = "PERCENT") Then
        scaledNewDiff = newDiff * 100#
    ElseIf (scale_ = "NONE") Then
        scaledNewDiff = newDiff
    'Else assume PPM
    Else: scaledNewDiff = newDiff * 1000000#
    End If
    invertDiff = scaledNewDiff / difference
End Function
