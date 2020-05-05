// initial Leaflet map options
const options = {
    zoomSnap: .1,
    zoomControl: false
};

// create Leaflet map and apply options
const map = L.map('map',options);
new L.control.zoom({position:"bottomright"}).addTo(map)

// set global variables for map layer,
// mapped attribute, attribute already normalized (percent)
let attributeValue = "TP";

// create object to hold legend titles
const labels = {
    "TP": "Total Phosphorus (mg/L)",
    "TN": "Total Nitrogen (mg/L)",
    "TD": "Turbidity",
    "CL":  "Chloride (mg/L)"
};

var Basin = $.getJSON("mapdata/majorbasin.geojson", function (basin) {
    // jQuery method uses AJAX request for the GeoJSON data
    const mbasin = L.geoJson(basin,{
        style:function style(feature) {
            return {
                fillColor: 'white',
                weight: 1,
                opacity: 1,
                color: 'white',
                //dashArray: '4',
                fillOpacity: 0.7
            };
        }
    }).addTo(map);

    // fit the map's bounds and zoom level using the counties extent
    map.fitBounds(mbasin.getBounds(), {
        padding: [18, 18] // add padding around counties
    });
});

var Sites = $.when(Basin).done(function () {
    $.getJSON("mapdata/sites.geojson", function (data) {
        // jQuery method uses AJAX request for the GeoJSON data
        console.log(data);
        // call draw map and send data as parameter
        drawMap(data);
    })
});





function drawMap(data) {
    // create Leaflet data layer and add to map
    const sites = L.geoJson(data, {
        pointToLayer: function (feature, latlng) {
            return L.circleMarker(latlng,);
        },
        style: function style(feature){
            return{
                radius: 8,
                fillColor: "#ffffff",
                color: "#000",
                weight: 1,
                opacity: 0.5,
                fillOpacity: 0.8
            };
        },
        // add hover/touch functionality to each feature layer
        onEachFeature: function (feature, layer) {

            // when mousing over a layer
            layer.on('mouseover', function () {

                // change the stroke color and bring that element to the front
                layer.setStyle({
                    color: '#ff6e00'
                }).bringToFront();
            });

            // on mousing off layer
            layer.on('mouseout', function () {

                // reset the layer style to its original stroke color
                layer.setStyle({
                    color: '#20282e'
                });
            });
        }
    }).addTo(map);



    updateMap(sites); // draw the map
    // //console.log(towns);
    addUi(sites);
    addLegend();

}

function updateMap(sites) {
    // logging sites to console here to
    // verify the Leaflet layers object is not accessible
    // and scoped within this function
    console.log(sites);

    // get the class breaks for the current data attribute
    const breaks = getClassBreaks(sites);

    // loop through each county layer to update the color and tooltip info
    sites.eachLayer(function (layer) {

        const props = layer.feature.properties;

        // set the fill color of layer based on its normalized data value
        layer.setStyle({
            fillColor: getColor(props[attributeValue], breaks, attributeValue)
        });

        // assemble string sequence of info for tooltip (end line break with + operator)
        let tooltipInfo = `<b>${props["name"]}</b></br>
            Value: ${(props[attributeValue]).toLocaleString()}`;

        // bind a tooltip to layer with county-specific information
        layer.bindTooltip(tooltipInfo, {
            // sticky property so tooltip follows the mouse
            sticky: true
        });

    });

// update the legend with the current data attribute information
    updateLegend(breaks);
    console.log(breaks);

}

// Get class breaks in data
function getClassBreaks(sites) {

    // create empty Array for storing values
    const values = [];

    // loop through all the sites
    sites.eachLayer(function (layer) {
        let value = layer.feature.properties[attributeValue];
        values.push(value); // push the value for each layer into the Array
    });

    values.sort(function(a,b){return a-b});

    // determine quantiles creates array of arrays
    const quantiles =  [ss.quantile(values,0.25),
                        ss.quantile(values,0.5),
                        ss.quantile(values,0.75),
                        ss.quantile(values,0.9)];

    // const lowerBreak = [];
    // for (i=0; i< 4; i++){
    //     var n = values.indexOf(quantiles[i]);
    //     lowerBreak.push(values[n+1])
    // }

    const breaks = [
                    [ss.min(values),quantiles[0]],
                    [quantiles[0]+0.001,quantiles[1]],
                    [quantiles[1]+0.001,quantiles[2]],
                    [quantiles[2]+0.001,quantiles[3]],
                    [quantiles[3]+0.001,ss.max(values)]];

    return breaks;

}


// Get color of parameter
function getColor(d, breaks, attribute) {
    // function accepts a single normalized data attribute value
    // and uses a series of conditional statements to determine which
    // which color value to return to return to the function caller
    if (attribute == "TP") {
        if (d <= breaks[0][1]) {
                return '#edf8fb';
            } else if (d <= breaks[1][1]) {
                return '#b2e2e2';
            } else if ( d <= breaks[2][1]) {
                return '#66c2a4';
            } else if (d <= breaks[3][1]) {
                return '#2ca25f'
            } else if (d <= breaks[4][1]) {
                return '#006d2c'
            }}


    if (attribute == "TN") {
        if (d <= breaks[0][1]) {
            return '#fee5d9';
        } else if (d <= breaks[1][1]) {
            return '#fcae91';
        } else if (d <= breaks[2][1]) {
            return '#fb6a4a'
        } else if (d <= breaks[3][1]) {
            return '#de2d26'
        } else if (d <= breaks[4][1]) {
            return '#a50f15'
        }}


    if (attribute == "TD") {
        if (d <= breaks[0][1]) {
            return '#feedde';
        } else if (d <= breaks[1][1]) {
            return '#fdbe85';
        } else if (d <= breaks[2][1]) {
            return '#fd8d3c'
        } else if (d <= breaks[3][1]) {
            return '#e6550d'
        } else if (d <= breaks[4][1]) {
            return '#a63603'
        }}

    if (attribute == "CL" ) {
        if (d <= breaks[0][1]) {
            return '#eff3ff';
        } else if (d <= breaks[1][1]) {
            return '#bdd7e7';
        } else if (d <= breaks[2][1]) {
            return '#6baed6'
        } else if (d <= breaks[3][1]) {
            return '#3182bd'
        } else if (d <= breaks[4][1]) {
            return '#08519c'
        }}
}


// Add legend to map
function addLegend() {

    // create a new Leaflet control object, and position it top left
    const legendControl = L.control({ position: 'topleft' });

    // when the legend is added to the map
    legendControl.onAdd = function() {

        // select a div element with an id attribute of legend
        const legend = L.DomUtil.get('legend');

        // disable scroll and click/touch on map when on legend
        L.DomEvent.disableScrollPropagation(legend);
        L.DomEvent.disableClickPropagation(legend);

        // return the selection to the method
        return legend;

    };

    // add the empty legend div to the map
    legendControl.addTo(map);
}

function updateLegend(breaks) {
    // select the legend, add a title, begin an unordered list and assign to a variable
    const legend = $('#legend').html(`<h5>${labels[attributeValue]}</h5>`);

    // loop through the Array of classification break values
    for (let i = 0; i <= breaks.length - 1; i++) {

        let color = getColor(breaks[i][0], breaks, attributeValue);

        legend.append(
            `<span style="background:${color}"></span>
      <label>${(breaks[i][0]).toLocaleString(undefined, {maximumFractionDigits: 3})} &mdash;
      ${(breaks[i][1]).toLocaleString(undefined, {maximumFractionDigits: 3})}</label><br>`);
    }

}

function addUi(sites) {
    // create the select control
    var selectControl = L.control({ position: "topright" });

    // when control is added
    selectControl.onAdd = function() {
        // get the element with id attribute of ui-controls
        return L.DomUtil.get("dropdown-ui");
    };
    // add the control to the map
    selectControl.addTo(map);

    $('#dropdown-ui select').change(function() {
        // change event for currently selected value
        attributeValue = this.value;

        updateMap(sites);
    });

}
