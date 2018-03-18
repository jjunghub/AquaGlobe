// [ ] : list
// { } : object = hash table = dictionary

var countries = {};
var flowmaps = {};

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
var flowmapBinding = {
  find: function(scope) {
    return $(scope).find('.flowmap-output');
  },
  getId: function(el) {
    var id = el.getAttribute('id');
    return id;
  },
  showProgress: function(el, complete) {
  },
  onValueChange: function(el, data) {
    var country_select = data[0][0];
    var networks = data[1];
    //var map;
    var colors = {};
    console.log(networks);
    var pathColorscale = d3.scaleLinear()
                          .domain(d3.extent(networks, function(d) {return d['width'];}))
                          .interpolate(d3.interpolateLab)
                          .range(['rgba(254, 220, 79, 0.3)', 'rgba(180, 52, 4, 0.8)']);
                          //.range(['rgba(127, 205, 187, 0.30)', 'rgba(8, 29,88, 0.6)']);

                          //.range(['rgba(254, 196, 80, 0.3)', 'rgba(254, 196, 80, 0.9)']);
                          
    function colorMaptoCountry() {
		  networks.forEach(myfunction = function (item){
		    //from country color
        //colors[item.from_country] = 'rgba(29, 145, 192, 1)';
        //colors[item.from_country] = 'rgba(64, 127, 86, 1)';
        colors[item.from_country] = 'rgba(32, 120, 180, 1)';

        //to country color
        //colors[item.to_country] = 'rgba(37, 37, 37, 1)';
        //colors[item.to_country] = 'rgba(29, 145, 210, 1)';
        colors[item.to_country] = 'rgba(250, 111, 151, 1)';

      });
    }
    colorMaptoCountry();
    
    // set view location
    var view_lng = country_select['lng']/2;//108.277199; 
    var view_lat = country_select['lat']/2;//14.058324;

    d3.select(el).selectAll('div.flowmap-container').remove();

    // first-time rendering
    d3.select(el).selectAll('div.flowmap-container').data([0]).enter()
      .append('div')
      .attr('class', 'flowmap-container')
      .style('height', '100%')
      .each(function() {
        // Initialize leaflet
        //var map = L.map(el).setView([view_lat, view_lng], 2);

        var map = L.map(this).setView([view_lat, view_lng], 2);
        // Dark version
        /*L.tileLayer('https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png', 
        {
	        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="http://cartodb.com/attributions">CartoDB</a>',
	        subdomains: 'abcd',
	        maxZoom: 19
        }).addTo(map);
        */
        // Gray version
        L.tileLayer('//cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png',
        {
          maxZoom: 18,
          attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, &copy; <a href="https://carto.com/attribution">CARTO</a>'
        }).addTo(map);
        
        //map.setView([view_lat, view_lng], 2);
        
        var info = L.control();
        info.onAdd = function (map) {
          this._div = L.DomUtil.create('div', 'info');
          this.update();
          return this._div;
        };
        
        function getFilteredNetworks(country){
            var filtered = networks.filter(function(datum){
              return datum.from_country === country || datum.to_country === country;
            }).sort(function(a, b){
              return d3.descending(+a['width'], +b['width']);
            });
            return filtered;
        }
        
        info.update = function (country) { 
          var filter_networks = getFilteredNetworks(country);
          var ntop = 10;
          var top_networks = filter_networks.slice(0, ntop);
          var info_top10 = "";
          if(el.getAttribute('id') == "flowmap0") {
            this._div.innerHTML = '<h4>'+ 'Finance networks related to ' + '<b>' + country + '</b>' + '</h4>' +
                                '<h5>'+ '<b>' + filter_networks.length + '</b>' + ' possible networks exist' + '</h5>';
            if(filter_networks.length > 10){
              this._div.innerHTML = this._div.innerHTML + '<b> Top 10 Possible Network List </b> <br />'
            }
            top_networks.forEach(function (item){
              if(item.from_country === country){
                  info_top10 = info_top10 + 'to ' + '<b>' + item.to_country + '</b>' + ', ' +  item['width'] + ' (ton/ year)' + '<br />';
              }
              else if(item.to_country === country){
                  info_top10 = info_top10 + 'from ' + '<b>' + item.from_country + '</b>' + ', ' +  item['width'] + ' (ton/ year)' + '<br />';
              }            
            });
            this._div.innerHTML = this._div.innerHTML + info_top10;
          }
          else if(el.getAttribute('id') == "flowmap1") {
            this._div.innerHTML = '<h4>'+ 'Culture & Technical Networks related to ' + '<b>' + country + '</b>' + '</h4>' +
            '<h5>'+ '<b>' + filter_networks.length + '</b>' + ' possible networks exist' + '</h5>';
            if(filter_networks.length > 10){
              this._div.innerHTML = this._div.innerHTML + '<b> Top 10 Possible Network List </b> <br />'
            }
            top_networks.forEach(function (item){
              if(item.from_country === country){
                  info_top10 = info_top10 + 'to ' + '<b>' + item.to_country + '</b>' + ', intensity of ' +  item['width'] + '<br />';
              }
              else if(item.to_country === country){
                  info_top10 = info_top10 + 'from ' + '<b>' + item.from_country + '</b>' + ', intensity of ' +  item['width'] + '<br />';
              }            
            });
            this._div.innerHTML = this._div.innerHTML + info_top10;
          }
        };

        function countryStyle(feature){
              return {
                'fillOpacity' : 0.3,
                'fillColor' : colors[feature.name] || 'none',
                'weight': 1.0,
                'color': colors[feature.name] ? 'rgba(0, 0, 0, 0.5)' : 'none'
              };
        }
        function onEachFeature(feature, layer) {
          layer.on({
            mouseover : highlightFeature,
            mouseout : resetHighLight,
            click : zoomToFeature
          });
        }
        function findRelatedCountires(name){
          var relatedCountries = {};
          var filter_networks = networks.filter(function(datum){
            return datum.from_country === name || datum.to_country === name;
          });
          filter_networks.forEach(function (item){
            relatedCountries[item.from_country] = true;
            relatedCountries[item.to_country] = true;
          });
          return relatedCountries;
        }
        function highlightFeature(e) {
          var layer = e.target;
          var relatedCountries = findRelatedCountires(layer.feature.name);
          geojson.eachLayer(function(layer){
            if(relatedCountries[layer.feature.name]){
              layer.setStyle({
                weight: 2,
                color: 'rgba(250, 0, 0, 0.5)',
                fillOpacity: 0.7
              });
            }
          });
          info.addTo(map);
          //var a = d3.select('.info').attr('text-align', 'center');
          //console.log(a);
          info.update(layer.feature.name);
        }
        function resetHighLight(e){
          geojson.eachLayer(function(layer){
            geojson.resetStyle(layer);
          });
          //geojson.resetStyle(e.target);
          info.remove();
        }
        function zoomToFeature(e){
          map.fitBounds(e.target.getBounds());
          //map.setZoom(4);
        }
        
        //console.log(countries.features);
				var geojson = L.geoJson(countries.features, {
				  style : countryStyle,
				  onEachFeature: onEachFeature
				}).addTo(map);



        // Initialize D3 custom layer
        function projectPoint(x, y) {
          var point = map.latLngToLayerPoint(new L.LatLng(y, x));
          this.stream.point(point.x, point.y);
        }
        var transform = d3.geoTransform({point : projectPoint});
        var path1 = d3.geoPath().projection(transform);
        
        var svg = d3.select(map.getPanes().overlayPane).append("svg")
                  .attr("pointer-events", "none");
        
        // For arrow
        var defs = svg.append("svg:defs");
        defs.append("svg:marker")
            .attr("id", "arrow_" + el.getAttribute('id'))
            .attr("refX", 2)
            .attr("refY", 0)
            .attr("viewBox", "0 -5 4 10")            					
            .attr("markerWidth", 2.5)
            .attr("markerHeight", 2.5)
            .attr("orient", "auto")
            .append("path")
					  .attr("d", "M0,-5L4,0L0,5")
					  .style("fill", function(data){
					    if(el.getAttribute('id') == "flowmap0") return 'rgba(153, 52, 4, 0.3)';
              else if(el.getAttribute('id') == "flowmap1") return 'rgba(153, 52, 4, 0.3)';
					  });
					  
				/*var fromcountryStyle = {
				  'stroke-width' : '0.5px',
				  'color' : 'rgba(0, 0, 0, 0.5)',
				  'fillColor' : 'rgba(29, 145, 192, 0.3)'
				};
				var tocountryStyle = {
				  'stroke-width' : '0.5px',
				  'color' : 'rgba(0, 0, 0, 0.5)',
				  'fillColor' : 'rgba(29, 145, 192, 0.3)'
				};
				*/
			

        var g = svg.append("g").attr("class", "leaflet-zoom-hide");
        /*
        var gCountries = g.append('g').attr('class', 'countries');
        var selCountries = gCountries.selectAll("path").data(countries.features).enter()
          .append("path")
          .attr("pointer-events", "all")
          .attr('stroke', function(feature){
            var colors = {};
            networks.forEach(function(item){
              colors[item.from_country] = 'rgba(0, 0, 0, 0.5)';
              colors[item.to_country] = 'rgba(0, 0, 0, 0.5)';
            });
            return colors[feature.name] || 'none';
          })
          .attr('fill', function(feature) {
            var colors = {};
            if(el.getAttribute('id')=="flowmap0"){
              networks.forEach(myfunction = function (item){
                colors[item.from_country] = 'rgba(29, 145, 192, 0.3)';
                colors[item.to_country] = 'rgba(37, 37, 37, 0.3)';
              });
            }
            else if(el.getAttribute('id')=="flowmap1"){
              networks.forEach(myfunction = function (item){
                colors[item.from_country] = 'rgba(64, 127, 86, 0.3)';
                colors[item.to_country] = 'rgba(29, 145, 192, 0.3)';
              });
            }
            //console.log(colors);
            return colors[feature.name] || 'none';
          });
        */

        var gRoutes = g.append('g').attr('class', 'routes');

        // Rerender custom layer
        function render() {
          // Set bounding box
          var bounds = path1.bounds(countries);
          console.log(bounds);
          var topLeft = bounds[0];
          var bottomRight = bounds[1];
          svg
            .attr("width", bottomRight[0] - topLeft[0])
            .attr("height", bottomRight[1] - topLeft[1])
            .style("left", topLeft[0] + "px")
            .style("top", topLeft[1] + "px");
          g
            .attr("transform", "translate(" + -topLeft[0] + "," + -topLeft[1] + ")");

          // Update countries
          //selCountries
          //  .attr("d", path);

          // Update routes
          function mytoArc(fromlng, fromlat, tolng, tolat) {
            var p0 = [fromlat, fromlng];
            var p2 = [tolat, tolng];
            var dist = Math.sqrt(Math.pow(p0[0] - p2[0], 2) + Math.pow(p0[1] - p2[1], 2));
            //약간 짧은 도착점으로
            p2[0] -= (p2[0]-p0[0])/dist * 1.0;
            p2[1] -= (p2[1]-p0[1])/dist * 1.0;
            
            dist = Math.sqrt(Math.pow(p0[0] - p2[0], 2) + Math.pow(p0[1] - p2[1], 2));
            var p1 = [(p0[0] + p2[0]) * 0.5, (p0[1] + p2[1]) * 0.5];
            p1[1] += dist * 0.2;

            return [p0, p1, p2];
          }
        
          var selRoutes = gRoutes.selectAll('path').data(networks);
          selRoutes.enter()
            .append('path')
            .merge(selRoutes)
            .attr('stroke-linecap', 'none')
            .attr('stroke', function(data) {
              return pathColorscale(data.width);
              //if(el.getAttribute('id') == "flowmap0") return 'rgba(245, 249, 118, 0.8)';
              //else if(el.getAttribute('id') == "flowmap1") return 'rgba(34, 94, 168, 0.8)';
            })
            .attr('stroke-width', function(data) {
              return 1.8;
              //if(el.getAttribute('id')=="flowmap0")return Math.log(data.width)*1.5;
              //else if(el.getAttribute('id')=="flowmap1")return Math.log(data.width)*1.5;        
            }) //data.width
            .attr('fill', 'none')
            .attr("marker-end", "url(#arrow_" + el.getAttribute('id') + ")")
            .attr('d', function(data) {
              var route = mytoArc(data.from_lng, data.from_lat, data.to_lng, data.to_lat);
              var pixels = route.map(function(r) {
                return map.latLngToLayerPoint(new L.LatLng(r[0], r[1]));
              });
              var line = d3.line()
                .x(function(d) {return d.x;})
                .y(function(d) {return d.y;})
                .curve(d3.curveBundle);
              return line(pixels);
            });
          selRoutes.exit().remove();
        }
        
        map.on('moveend', render);
        render();
      });
  },
  onValueError: function(el, err) {
    console.log(['onValueError', el, err]);
  }
};

Shiny.outputBindings.register(flowmapBinding, "ak.flowmapBinding");
