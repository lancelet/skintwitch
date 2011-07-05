#include "filterwidth.h"
#include "patterns.h"

surface skingrid (
  /* normal ambient and diffuse coefficients */
  uniform float Ka = 1;
  uniform float Kd = 1;
  /* point cloud ambient occlusion */
  uniform string ptCloudName = "";
  uniform float Ko = 1;
  uniform float baking = 0;
  /* grid parameters */
  uniform float nMajorUGridLines = 8;
  uniform float nMajorVGridLines = 7;
  uniform float majorGridLineFraction = 0.005;
  uniform float nMinorGridLines = 5;
  uniform float minorGridLineFraction = 0.001;
  uniform color majorGridLineColor = color(0.110, 0.459, 0.463);
  uniform color minorGridLineColor = color(0.110, 0.459, 0.463);
  uniform color backgroundColor = color(0.753, 0.933, 0.949);
  uniform float bgOpacity = 0.2;
) {
  point Nf = faceforward(normalize(N), I);
  
  /* ambient occlusion */
  float ambo = 1.0;
  if (ptCloudName != "" && baking == 0) {
    ambo = 1.0 - occlusion(P, Nf, 0, "filename", ptCloudName);
  }

  float fwu = filterwidth(u);
  float fwv = filterwidth(v);

  /* major grid lines */
  float majorUPeriod = (1 + majorGridLineFraction) / (nMajorUGridLines - 1);
  float majorVPeriod = (1 + majorGridLineFraction) / (nMajorVGridLines - 1);
  float majorEdgeU = majorUPeriod - majorGridLineFraction;
  float majorEdgeV = majorVPeriod - majorGridLineFraction;
  float majorGridU = filteredpulsetrain(majorEdgeU, majorUPeriod, u, fwu);
  float majorGridV = filteredpulsetrain(majorEdgeV, majorVPeriod, v, fwv);
  float majorGrid = clamp(majorGridU + majorGridV, 0, 1);

  /* minor grid lines */
  float nMinorUGridLines = (nMajorUGridLines - 1) * nMinorGridLines + 1;
  float nMinorVGridLines = (nMajorVGridLines - 1) * nMinorGridLines + 1;
  float minorUPeriod = (1 + minorGridLineFraction) / (nMinorUGridLines - 1);
  float minorVPeriod = (1 + minorGridLineFraction) / (nMinorVGridLines - 1);
  float minorEdgeU = minorUPeriod - minorGridLineFraction;
  float minorEdgeV = minorVPeriod - minorGridLineFraction;
  float minorGridU = filteredpulsetrain(minorEdgeU, minorUPeriod, u, fwu);
  float minorGridV = filteredpulsetrain(minorEdgeV, minorVPeriod, v, fwv); 
  float minorGrid = clamp(minorGridU + minorGridV, 0, 1);

  /* edge of the grid */
  float edgeUFraction = majorGridLineFraction;
  float edgeVFraction = majorGridLineFraction;
  float edgeU = 1 - (filterstep(edgeUFraction, s) - filterstep(1 - edgeUFraction, s));
  float edgeV = 1 - (filterstep(edgeVFraction, v) - filterstep(1 - edgeVFraction, v));
  float edge = clamp(edgeU + edgeV, 0, 1);

  /* grid = major + minor + edge */
  float grid = clamp(majorGrid + minorGrid + edge, 0, 1);

  /* surface color */
  color surfCol = mix(
    backgroundColor,
    mix(minorGridLineColor, majorGridLineColor, majorGrid),
    grid
  );
  
  /* surface opacity */
  float surfOpac = mix(bgOpacity, 1, grid);

  /* surface shading */
  color opac = Os * surfOpac;
  color col = opac * surfCol * (Ka * ambient() + Kd * diffuse(Nf) + Ko * ambo);
  Oi = opac;
  Ci = col;
  if (ptCloudName != "" && baking == 1 && opac > 0) {
    bake3d(ptCloudName, "", P, Nf, "_area", area(P) * opac, "_radiosity", Ci);
  }
}
