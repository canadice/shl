## ELI5 or tldr

I see how you look like and place you in the sandbox close to your
lookalike. The closer you are, the more you look alike.

## Introduction

As every player is defined by more than 20 different attributes, it is
difficult to try and compare and visualize players with each other in a
simple manner. Fortunately a method called *multidimensional scaling*
allows us to reduce the number of dimensions (or attributes) in order to
facilitate this goal.

Reducing the number of dimension too much risks reducing the data to a
form that summarizes or hides part of the information the data holds. On
the other hand not reducing the number of dimensions enough still
produces problems with visualization and interpretation.

If we focus on visualization, a 2- or 3 dimensional plot provides a
result that is easily created, however the interpretation of this plot
might still prove difficult.

## The setup to multidimensional scaling

In order to reduce the number of dimension we must start with defining
how similar each player is to one another, as this relationship is what
we want to keep. As the player ratings are numerical values, this
becomes relatively easy by calculating a pairwise distance value between
the players.

### Distance metrics

We can look at two different types of distance metrics that can be used
to calculate the distance values.

First we have the *Euclidean* distance that calculates the **closest**
distance between two points, as seen in figure below. Hope you remember
your high school geography because the distance is calculated as the
hypotenuse of the side lengths of the right angle triangle that is
formed from the points. The distance in figure is thereby calculated as
$\sqrt{1^2 + 1^2} = \sqrt{2} \approx 1.41$.

    ## Warning in geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), color = "black", : All aesthetics have length 1, but the data has 2 rows.
    ## ℹ Please consider using `annotate()` or provide this layer with data containing
    ##   a single row.

    ## Warning in geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1), color = "black", : All aesthetics have length 1, but the data has 2 rows.
    ## ℹ Please consider using `annotate()` or provide this layer with data containing
    ##   a single row.

<figure>
<img
src="D:/GitHubs/shl_API/SHL-Analytics/app-documents/mdExplanation_files/figure-markdown_strict/euclidean-1.png"
alt="Euclidean distance between two points" />
<figcaption aria-hidden="true">Euclidean distance between two
points</figcaption>
</figure>

This measure assumes that it is possible to place values between the
integers (whole numbers), which is not the case if the variable is
discrete. We must then restrict that the distance between two points
follows the whole numbers. A practical example of this issue is if you
would want to calculate the distance you would need to walk between two
streets and avenues in Manhattan, for instance the corner of the 50th
Street and 3rd Avenue, and 51th Street and 2nd Avenue. The Euclidean
distance assumes that you are able to walk between the corners directly,
but if you’ve ever been in Manhattan, you would notice that there are
some rather large buildings in the way. You must walk along the Street
to another corner before walking along the Avenue to your destination.

<!-- ```{r, message=FALSE} -->
<!-- ggmap(get_stamenmap(bbox = c(-73.971,40.7530,-73.965,40.759), zoom = 15, messaging = FALSE)) + -->
<!--   theme_bw() -->
<!-- ``` -->

This practical example shows how the *Manhattan* distance is calculated,
by calculating the number of corners between your current position and
your destination. The distance calculation would then be the sum of all
the lengths between the two points, in the case of the figure below:
1 + 1 = 2.

    ## Warning in geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), color = "red", : All aesthetics have length 1, but the data has 2 rows.
    ## ℹ Please consider using `annotate()` or provide this layer with data containing
    ##   a single row.

    ## Warning in geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1), color = "red", : All aesthetics have length 1, but the data has 2 rows.
    ## ℹ Please consider using `annotate()` or provide this layer with data containing
    ##   a single row.

<figure>
<img
src="D:/GitHubs/shl_API/SHL-Analytics/app-documents/mdExplanation_files/figure-markdown_strict/manhattan-1.png"
alt="Euclidean distance between two points" />
<figcaption aria-hidden="true">Euclidean distance between two
points</figcaption>
</figure>

We can also calculate the distance as the sum of the individual
differences for each dimension, in this case the difference in *x* from
0 to 1, and difference in *y* from 0 to 1: (1 − 0) + (1 − 0) = 2

### Calculating the distances

In the case of player ratings, they are all integers between 5 and 20,
with some limitations for specific ratings. We now want to calculate the
similarity of them, where players that have similar ratings also will
end up closer together in the final plot. A similarity can also be
considered as an inverse distance, where high similarity is equal to
small distance and vice versa. As shown above, the *Manhattan* distance
is to be used when we have discrete variables, so this calculation can
be done with the following example data:

<table>
<caption>Example data used</caption>
<colgroup>
<col style="width: 12%" />
<col style="width: 8%" />
<col style="width: 10%" />
<col style="width: 6%" />
<col style="width: 10%" />
<col style="width: 14%" />
<col style="width: 11%" />
<col style="width: 11%" />
<col style="width: 7%" />
<col style="width: 6%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">screening</th>
<th style="text-align: left;">gettingOpen</th>
<th style="text-align: left;">passing</th>
<th style="text-align: left;">puckhandling</th>
<th style="text-align: left;">shootingAccuracy</th>
<th style="text-align: left;">shootingRange</th>
<th style="text-align: left;">offensiveRead</th>
<th style="text-align: left;">checking</th>
<th style="text-align: left;">hitting</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Poopity Scoop</td>
<td style="text-align: left;">5</td>
<td style="text-align: left;">15</td>
<td style="text-align: left;">15</td>
<td style="text-align: left;">15</td>
<td style="text-align: left;">11</td>
<td style="text-align: left;">15</td>
<td style="text-align: left;">15</td>
<td style="text-align: left;">15</td>
<td style="text-align: left;">14</td>
</tr>
<tr class="even">
<td style="text-align: left;">Cassius Darrow</td>
<td style="text-align: left;">5</td>
<td style="text-align: left;">17</td>
<td style="text-align: left;">17</td>
<td style="text-align: left;">18</td>
<td style="text-align: left;">12</td>
<td style="text-align: left;">15</td>
<td style="text-align: left;">17</td>
<td style="text-align: left;">18</td>
<td style="text-align: left;">13</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Difference</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">0</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">1</td>
</tr>
</tbody>
</table>

Example data used

The last row contains the the absolute differences between the two
players, i.e. how many “Manhattan intersections” between them. If we
tally that row of differences we would get the total distance (or
inverse similarity) between the two players: 24.

This calculation is then done with every player to produce a distance
matrix that contain all pairwise calculated distances of all players in
the data. The diagonal of the matrix will have a distance of 0 as the
calculation is based on the same player.

<table>
<caption>Distance matrix based on 5 players</caption>
<colgroup>
<col style="width: 20%" />
<col style="width: 14%" />
<col style="width: 15%" />
<col style="width: 20%" />
<col style="width: 15%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">Poopity Scoop</th>
<th style="text-align: right;">Cassius Darrow</th>
<th style="text-align: right;">Satoshi Zizagooney</th>
<th style="text-align: right;">Nicolaj Muller</th>
<th style="text-align: right;">Bobby Sharp</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Poopity Scoop</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">24</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">34</td>
<td style="text-align: right;">56</td>
</tr>
<tr class="even">
<td style="text-align: left;">Cassius Darrow</td>
<td style="text-align: right;">24</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">50</td>
<td style="text-align: right;">50</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Satoshi Zizagooney</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">43</td>
<td style="text-align: right;">47</td>
</tr>
<tr class="even">
<td style="text-align: left;">Nicolaj Muller</td>
<td style="text-align: right;">34</td>
<td style="text-align: right;">50</td>
<td style="text-align: right;">43</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">44</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Bobby Sharp</td>
<td style="text-align: right;">56</td>
<td style="text-align: right;">50</td>
<td style="text-align: right;">47</td>
<td style="text-align: right;">44</td>
<td style="text-align: right;">0</td>
</tr>
</tbody>
</table>

Distance matrix based on 5 players

## Reducing the dimensions

After the distances between the players have been calculated, we now
know the relationship we want to visualize and interpret.
Multidimensional scaling tries to find a set of points in *k* dimensions
that equally represent the relationships seen in the *n* (number of
attributes) dimensions. We don’t need to get into the details of how
this is performed but the algorithm usually performs some form of
optimization to reduce the error between the observed distance matrix
and the distances from the projected new dimensions.

A classic example of this is how you with direct distances between
different US cities can produce a map that somewhat corresponds to the
real world, without providing any coordinates or similar to the method.

<table>
<caption>Five cities and their direct distance to one another</caption>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">Atlanta</th>
<th style="text-align: right;">Chicago</th>
<th style="text-align: right;">Denver</th>
<th style="text-align: right;">Houston</th>
<th style="text-align: right;">LosAngeles</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Atlanta</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">587</td>
<td style="text-align: right;">1212</td>
<td style="text-align: right;">701</td>
<td style="text-align: right;">1936</td>
</tr>
<tr class="even">
<td style="text-align: left;">Chicago</td>
<td style="text-align: right;">587</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">920</td>
<td style="text-align: right;">940</td>
<td style="text-align: right;">1745</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Denver</td>
<td style="text-align: right;">1212</td>
<td style="text-align: right;">920</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">879</td>
<td style="text-align: right;">831</td>
</tr>
<tr class="even">
<td style="text-align: left;">Houston</td>
<td style="text-align: right;">701</td>
<td style="text-align: right;">940</td>
<td style="text-align: right;">879</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">1374</td>
</tr>
<tr class="odd">
<td style="text-align: left;">LosAngeles</td>
<td style="text-align: right;">1936</td>
<td style="text-align: right;">1745</td>
<td style="text-align: right;">831</td>
<td style="text-align: right;">1374</td>
<td style="text-align: right;">0</td>
</tr>
</tbody>
</table>

Five cities and their direct distance to one another

<img src="D:/GitHubs/shl_API/SHL-Analytics/app-documents/mdExplanation_files/figure-markdown_strict/unnamed-chunk-3-1.png" alt="Map of the United States"  />
<p class="caption">
Map of the United States
</p>

The dimensions of the map does not exactly correspond to the longitude
and latitude but they are somewhat representative of those measures. The
map produced isn’t perfect, for instance it is upside down and the
cities located on the corners of the map might not directly correspond
to their geographic location. However the map was produced with only the
calculated direct distances between the different and nothing else,
which shows the value of the method.

In the case of the map the two dimensions can be interpreted relatively
easy, as a representation of the geographical coordinates. However in
the case of the players, the reduced dimensions are not that easily
interpreted as something that relates to the data. This produces some
difficulty in determining what distinguishes players in one area of the
plot from other areas, but the visual representation still shows how
similar players in the league are to one another.

<img src="D:/GitHubs/shl_API/SHL-Analytics/app-documents/mdExplanation_files/figure-markdown_strict/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
