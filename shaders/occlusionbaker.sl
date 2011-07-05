surface occlusionbaker(
  string ptCloudName = "";
) {
  point Nf = faceforward(normalize(N), I);
  color col = 1;
  if (ptCloudName != "") {
    bake3d(ptCloudName, "", P, Nf, "_area", area(P), "_radiosity", col);
  }
  Oi = Os;
  Ci = Os * (-Nf.normalize(I));
}
