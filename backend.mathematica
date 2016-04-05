ImageFusion[{img1_, wts1_}, {img2_, wts2_}, index_: Automatic, wH_] :=
  Module[{i1 = img1, i2 = img2, dwd, wcoeff, nwcoeff, nimg, wind,
   rnew},
  i1 = Import[i1]; i2 = Import[i2];
  wts1 = Interpreter["Number"][wts1];
  wts2 = Interpreter["Number"][wts2];
  Which[wH == 0,
   i1 = HistogramTransform[ConformImages[{i1, i2}][[1]],
     ConformImages[{i1, i2}][[2]]], wH == 1,
   i2 = HistogramTransform[ConformImages[{i2, i1}][[1]],
     ConformImages[{i2, i1}][[2]]]];
  dwd = Map[
    StationaryWaveletTransform[#, CDFWavelet[], 3] &, {i1, i2}];
  wcoeff = #[index, {"Image", "ImageFunction" -> Identity}] & /@ dwd;
  nwcoeff =
   MapThread[
    First[#1] ->
      ImageAdd[ImageMultiply[Last[#1], wts1],
       ImageMultiply[Last[#2], wts2]] &, wcoeff];
  nwcoeff =
   Append[nwcoeff,
    First[dwd[[1]][{0, 0, 0}, {"Image",
       "ImageFunction" -> (ImageMultiply[#, wts1] &)}]]];
  InverseWaveletTransform[
   DiscreteWaveletData[nwcoeff, CDFWavelet[],
    StationaryWaveletTransform]]];
