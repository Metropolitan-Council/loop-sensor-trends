
**This app is in active development and will be updated frequently.
Thank you for your patience.**

## About

A Metropolitan Council analysis of over 1,000 freeway traffic monitoring
stations across the metropolitan area and beyond show that Minnesotans
are doing their part to limit the spread of COVID-19 by reducing travel
and staying home.

Researchers at the Metropolitan Council are using traffic data from the
Minnesota Department of Transportation (MnDOT) to evaluate the impact of
recent physical distancing efforts on regional and statewide travel.
Using a modeling approach that relies on historical traffic data to
estimate typical travel, Met Council researchers have shown that travel
across the region’s freeways has declined by more than 70% in the weeks
following the first COVID-19 case in Minnesota, on March 6th.

### State actions appear in travel data

Travel decreased over time as state officials took action to support
physical distancing efforts. For example, after the Governor asked
Minnesotans to cancel all large gatherings and limit restaurants to
takeout on March 18, travel decreased by an additional 7% over what had
already been a 35% decrease from typical. Travel increased slightly on
Friday, March 20, perhaps in anticipation of additional statewide travel
restrictions.

These data are helping inform statewide models of disease transmission,
and helping policymakers evaluate the impact of their actions on
physical distancing efforts.

Researchers at the Met Council will continue to update these figures as
additional restrictions go into effect, and as restrictions are lifted.
Future research at the Council aims to understand the impact of physical
distancing efforts on other types of travel, especially pedestrian and
bike travel. Researchers also hope to study other aspects of travel
behavior – including the travel that Minnesotans do in service to others
in their household, family or community.

<img src="traffic-trends-actions.png" alt="Static plot of the percent difference in observed traffic level from
the expected traffic level" style="width: 100%">

### Data sources

Traffic data are provided by MnDOT. We pull these data using our
open-source R package,
[tc.sensors](https://github.com/Metropolitan-Council/tc.sensors).

### Modeling

To estimate typical traffic in a way that is robust to Minnesota’s
famous fluctuations in weather, we used a statistical modeling approach
that relies on three years of data (2018, 2019, and 2020 up to March 1,
2020). The model used here is a generalized additive model, or GAM. GAMs
are commonly used in analyses of data with a strong seasonal or cyclical
trend – especially when that trend does not follow a perfectly straight
line. Some GAMs that people might already be familiar with are those
that meteorologists and climatologists use to estimate temperature
trends within and across years.

Our GAMs consider two trends. One trend happens over the span of a year:
in most places, travel increases in the summer months and decreases in
the winter months. A second trend occurs over the span of a week: travel
tends to be highest on Fridays, and lowest on Sundays. To allow the
shapes of these yearly and weekly trends to vary in a location-specific
way, we created separate models for each traffic monitoring station.

#### May 2020 state model update

Starting May 6, 2020, the model for state data was modified to better
reflect expected average conditons. Minnesota experienced significant
weather events on two Thursdays in April 2019, which created a low
historical baseline to compare current traffic levels against. The
result was a large outlier on Thursday, April 30. To remedy this issue,
daily counter traffic is now compared against the month-day-of-week
average from up to 4 years of data (2016-2019) depending on the
consistency of traffic and data availability over the time period.

The plot below shows how the model update changed the difference in
expected traffic levels for statewide sensors. There are relatively
small changes for dates, and higher traffic value for Thursday, April 30
is reduced.

<img src="mndot-comparison.png" alt="Static plot showing the change traffic reduction when using the May 6, 2020 state model update and the previous state model." style="width: 70%;margin-left: 15%;margin-right: 15%;">

### Resources

[Minnestota COVID-19 Response home
page](https://mn.gov/governor/covid-19/)  
[Minnestoa Department of Health COVID-19 home
page](https://www.health.state.mn.us/diseases/coronavirus/index.html)

#### Other data tools

[Minnesota COVID-19 Public
Dashboard](https://mndps.maps.arcgis.com/apps/opsdashboard/index.html#/f28f84968c1148129932c3bebb1d3a1a)  
[Minnesota COVID-19 Response and Preparation Capacity
dashboard](https://mn.gov/covid19/data/response-prep/index.jsp)

### Contributors

  - Liz Roten, app development (Metropolitan Council)  
  - Ashley Asmus, data and model development (Metropolitan Council)  
  - Brian Kary, data development and ongoing consultation (MnDOT)  
  - Ian Vaagenes, data and model development (MnDOT)  
  - Jonathan Ehrlich, project management (Metropolitan Council)

### Contact

Email Sara Maaske at <Sara.Maaske@metc.state.mn.us>.

For app bug reports and new feature requests, feel free to open an Issue
on our [GitHub
repository](https://github.com/Metropolitan-Council/loop-sensor-trends)
for this project.

<right style="font-size: 1rem; text-align: right; display: block;">
*Last updated 2020-11-03*  
Build ID: 2020-11-03.roten.2cfae4ee  
</right>
