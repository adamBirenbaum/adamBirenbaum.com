+++
title = "Efficiently Scrape all Restaurant data in a County "

date = 2016-04-20T00:00:00
lastmod = 2018-01-13T00:00:00
draft = false

tags = ["academic"]
summary = "To understand recursion, you must first understand recursion"

[header]
image = "headers/madison-eats_blog_header.png"
caption = "Image credit: [**Academic**](https://github.com/gcushen/hugo-academic/)"

[[gallery_item]]
album = "1"
image = "https://raw.githubusercontent.com/gcushen/hugo-academic/master/images/theme-default.png"
caption = "Default"

[[gallery_item]]
album = "1"
image = "https://raw.githubusercontent.com/gcushen/hugo-academic/master/images/theme-ocean.png"
caption = "Ocean"

[[gallery_item]]
album = "1"
image = "https://raw.githubusercontent.com/gcushen/hugo-academic/master/images/theme-dark.png"
caption = "Dark"

[[gallery_item]]
album = "1"
image = "https://raw.githubusercontent.com/gcushen/hugo-academic/master/images/theme-forest.png"
caption = "Default"

[[gallery_item]]
album = "1"
image = "https://raw.githubusercontent.com/gcushen/hugo-academic/master/images/theme-coffee-playfair.png"
caption = "Coffee theme with Playfair font"

[[gallery_item]]
album = "1"
image = "https://raw.githubusercontent.com/gcushen/hugo-academic/master/images/theme-1950s.png"
caption = "1950s"
+++

While developing my restaurant mapping project, [Madison eats](http://adambirenbaum.com/project/madison-eats/), I came across an interesting problem.  I intended to use the API from a popular restaurant review website to populate my application, but like many public API's there were reasonable limits in place.  Given an exact location and radius, the api would return a maximum of 50 restaurants per call with 5,000 calls allowed per day.  With these constraints, *what is the best method to get the data for all restaurants in a region*.

The problem is analogous to dropping circuluar fishing nets in the ocean.  If each net has a maximum capacity of 50 fish and their radius is allowed to vary, what strategy will allow you to catch the most fish using the fewest nets?  

To use the absolute fewest nets, you would need some prior knowlegdge of the fish density in the region.  What regions are densely populated with fish (you'll need a small net) and what regions are sparse (you can use a larger net)?  With that information, this becomes a simple optimaztion problem to pick a precise net size and location to catch all the fish with relatively few nets... but what if you don't know where the fish are ahead of time?

For my project, I know the region's coordinates, but I don't have any information on the number of restaurants or their location within that region.  In fact, the whole purpose of this data extraction process is to get the location of the restaurants along with some of its attributes.  Clearly, the ideal strategy as described with the fish is not possible.  

A simple, brute force method would be to pick a constant search radius, start at one corner of the region and iterate across the region, weaving back and forth just as you would when mowing the lawn.  Picking a proper search radius is an obvious problem with this strategy.  If you chose a radius, implemented the method and found that some calls included 50 restaurants, then either that search region happened to have exactly 50 restaurants, or more likely there were more than 50 and some restaurants were left out.  You would then have to decrease your search radius and try again, hoping there wasn't a search region with more than 50.  To fullfill this method with the fewest amount of API calls you would need to specify a search region small enough to include all of the restaurants in the most densely populated area of the overall region.  This of course means that in the sparsely populated regions you would make API calls in search areas with very few or no restaurants.

Embarassingly, I used this brute force method when I initially attempted to get the restaurant data in Dane county and it required more than 4,000 calls.  To put in respective how inefficent this method is, with 4,000 calls one could extract a maximum of 200,000 restaurants.  There are just over 1,200 restaurants in Dane county, nowhere near 200,000.

The much better solution I came up with is to use recursion.  You first divide up the overall region into a grid of four equally sized rectangles or squares.  Then make an api call in the center of the top-left rectangle with a radius such that it just barely includes the entire rectangle.  Find the number of restaurants returned from that region.  If there are less than 50, you move to the next rectangle.  If there are more than 50, subdivide that top-left rectangle into four more regions and repeat.  This strategy uses very few search regions for sparsely populated areas.  For more densely populated areas, this strategy will need to waste some API calls before its search region decreases enough such that there are fewer than 50 restaurants within it.  For example lets say there are 200 restaurants just in the top-left corner of the subdivided region.  That top-left region may need to be divided three or four times before the search region is small enough, and therefore those first three or four api calls were essentially a waste.  When no prior knowledge is had, this "waste" is a sacrifice well worth making.

Using this recursive method, it took about 300 api calls to get all of the restaurant data in Dane County, more than 13 times fewer than the brute force method.

[here](~/Downloads/Dane-WI_map.html)
