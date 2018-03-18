// [ ] : list
// { } : object = hash table = dictionary

var countries = {};
var bubbles = {};
var scale_r = 4;

// Load country boundaries and names
d3.queue()
  .defer(d3.json, 'data/newtopo_countries-50m.topo.json')
  .defer(d3.tsv, 'data/country-names_modify.tsv')
  .awaitAll(function(err, data) {
    // Extract polygons
    countries = topojson.feature(data[0], data[0].objects.countries);

    // Assign country names to each polygons
    var idToNameMap = {};
    var names = data[1];
    names.forEach(function(name) {
      idToNameMap[name.id] = name.name;
    });
    countries.features.forEach(function(feature) {
      feature.name = idToNameMap[feature.id];
    });
  });

// Define R shiny custom output binding

//s1 내가 고친부분
var bubbleBinding = {
  find: function(scope) {
    return $(scope).find('.bubblemap-output');
  },
  getId : function(el){
    var id = el.getAttribute('id');
    return id;
  },
  showProgress: function(el, complete) {
  },
  onValueChange: function(el, data) {
    var view_lng = 108;                   // lng of vietnam
    var view_lat = 14;                    // lat of vietnam

    function projectPoint(x, y) {
      var point = el._map.latLngToLayerPoint(new L.LatLng(y, x));
      this.stream.point(point.x, point.y);
    }
    var path = d3.geoPath().projection(d3.geoTransform({point : projectPoint}));

    // first rendering
    var mapContainer = d3.select(el).selectAll('div.bubblemap-container').data([0]);
    mapContainer.enter()
      .append('div')
      .attr('class', 'bubblemap-container')
      .style('height', '100%')
      .each(function() {
        // Initialize leaflet
        
        // Add popup interaction 
        var map = L.map(this);
        el._map = map;

        L.tileLayer('//cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png',
        {
          maxZoom: 18,
          attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, &copy; <a href="https://carto.com/attribution">CARTO</a>'
        }).addTo(map);
        
        map.setView([view_lat, view_lng], 3);

        // Initialize D3 custom layer
        var svg = d3.select(map.getPanes().overlayPane).append("svg");
        var g = svg.append("g").attr("class", "leaflet-zoom-hide");
        g.append('g').attr('class', 'countries');
        g.append('g').attr('class','circles');

        map.on('moveend', render);
        render();
      })
      .merge(mapContainer)
      .each(render);
      
      function render() {
        var map = el._map;
        var svg = d3.select(map.getPanes().overlayPane).select('svg');

        var countryNames = d3.set(data.map(function(d) {return d.country;}));
        var gCountries = svg.select('g.countries');
        var selCountries = gCountries.selectAll("path").data(countries.features);
        selCountries.enter()
          // enter
          .append("path")
          .merge(selCountries)
          // update
          .attr("d", path)
          .attr('fill', 'none')
          .attr('stroke', function(feature) {
            return countryNames.has(feature.name) ? 'rgba(0, 0, 0, 1)' : 'none';
          });
        selCountries.exit()
          // exit
          .remove();

        var bounds = path.bounds(countries);
        var topLeft = bounds[0];
        var bottomRight = bounds[1];
        svg
          .attr("width", bottomRight[0] - topLeft[0])
          .attr("height", bottomRight[1] - topLeft[1])
          .style("left", topLeft[0] + "px")
          .style("top", topLeft[1] + "px");
        
        var g = svg.select('g.leaflet-zoom-hide');
        g
          .attr("transform", "translate(" + -topLeft[0] + "," + -topLeft[1] + ")");

        var colorScale = d3.scaleLinear()
          .interpolate(d3.interpolateLab)
          .domain([0, d3.max(data, function(d) {return d.radius;})])
          .range(['rgba(0, 0, 180,0.3)', 'rgba(0, 0, 180,0.3)']);

        function stroke_color(x) {
          if( x === "new" ){
              return "rgba(255,255,0,1)";
          }
          else if(x === "dangerous"){
            return "rgba(255,0,0,1)";
          }
          else { return "black" ;}
        }
      
        var gCircles = svg.selectAll('g.circles');
        var circles = gCircles.selectAll('circle').data(data);
        circles.enter()
          .append('circle')
          .attr('r', 0)
          .attr('fill', colorScale(0))
          .merge(circles)
          .attr('cx', function(d) { return map.latLngToLayerPoint(new L.LatLng(d.lat, d.lng)).x;})
          .attr('cy', function(d) { return map.latLngToLayerPoint(new L.LatLng(d.lat,d.lng)).y;})
          .transition().duration(500)
          .attr('r', function(d) { return (d.radius)*scale_r; })
//          .attr('fill', function(d) { return colorcode(d.label); });
          .attr('fill', function(d) { return colorScale(d.radius);})
          .attr('stroke', function(d) { return stroke_color(d.label);})
          .attr('stroke-width', 2);
          
        circles.exit()
          .transition().duration(500)
          .attr('r', 0)
          .attr('fill', colorScale(0))
          .remove();
      }
    },
  onValueError: function(el, err) {
    console.log(['onValueError', el, err]);
  }
};

Shiny.outputBindings.register(bubbleBinding, "ak.bubbleBinding");