d=Re[Zeta[1/2+I Range[0,100,0.01]]];
cwd=ContinuousWaveletTransform[d,GaborWavelet[6],{4,12},WaveletScale->100];
scal=WaveletScalogram[cwd,All,Abs,ColorFunction->"CherryTones",Frame->False,PlotRangePadding -> 0,Axes->False,AspectRatio->1/1.75,Background->None];
Image[Rasterize[scal,RasterSize->300],ImageSize->{1008,576}]
