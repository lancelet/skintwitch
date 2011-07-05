surface matteocclusion(
  float Ka = 1;
  float Kd = 1;
  string ptCloudName = "";
  float Ko = 1;
  float baking = 0;
) {
  point Nf = faceforward(normalize(N), I);
  
  float ambo = 1.0;
  if (ptCloudName != "" && baking == 0) {
    ambo = 1.0 - occlusion(P, Nf, 0, "filename", ptCloudName);
  }
  
  Oi = Os;
  color col = Os * Cs * (Ka * ambient() + Kd * diffuse(Nf) + Ko * ambo);
  Ci = col;
  if (ptCloudName != "" && baking == 1) {
    bake3d(ptCloudName, "", P, Nf, "_area", area(P), "_radiosity", col);
  }
}
