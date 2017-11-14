
# Skyculture Blank

This is a program to produce a blank skyculture.  Skyculture is a format
used by the desktop planetarium program Stellarium to hold artwork for
constellations.  Skyculture format also has the advantages of simplicity,
comprehensiveness, and being open, so it can be straightforwardly
converted to other formats when needed.

Nevertheless, there is a certain amount of difficulty in producing a
skyculture, and this project seeks to assist with that by providing tools
to make a blank that generates the boilerplate of the format for you, as
well as template images that an artist can use as guides in order to
produce new constellation artwork.

This program is a work in progress.  It is currently able to make a
skyculture template of the 88 IAU constellations (including the special
combinations Ophiuchus + Serpens and Argo Navis).  Future work is planned
to produce chart sets for other traditions which do not line up with the
IAU constellations.  Also, not all charts map to the sphere as well as
they probably could, so I am continuing to tweak them.

## Data

To use skyculture-blank, you will need to obtain some astronomical catalog
data.  On unix-like machines, make sure that you have git and wget, and do
the following:

    cd data
    ./git-clone-hyg-database
    cd constellations-pbarbier
    ./fetch-constellation-data
    cd ../..

On other platforms, or if git and wget are unavailable, refer to the
source code of the two scripts mentioned above for the URLs at which to
obtain the needed data.

