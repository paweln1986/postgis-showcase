import * as leaflet from 'leaflet';
import 'leaflet-draw';
import 'leaflet-semicircle';

var geojsonMarkerOptions = {
    radius: 8,
    fillColor: "#ff7800",
    color: "#000",
    weight: 1,
    opacity: 0.1,
    fillOpacity: 0.8
};

const antennaTypeToColor = type => {
    switch(type) {
        case 'GSM': return '#a87932'
        case 'WCDMA': return '#89a832'
        case 'LTE': return '#40a832'
        case 'FiveG': return '#325ba8'
    }
}

export const map = (cssClass) => (onMapLoad) => (onPolygonUpdate) => (onPolygonDelete) => () => {
    let m = L.map(cssClass)
    m.on('load zoomend moveend', function (e) {
        console.log("loading data")
        onMapLoad(m.getBounds().toBBoxString())(m.getZoom())();
    });
    m.setView([51.06, 17.01], 11);
    
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        minZoom: 3,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(m);
    var geoJsonLayer = L.geoJSON([],{
        pointToLayer: function (feature, latlng) {
            if(feature.properties.tag === 'SingleAntenna'){
                return L.semiCircle(latlng, {
                    color: antennaTypeToColor(feature.properties.antenna_type),
                    fillColor: antennaTypeToColor(feature.properties.antenna_type), 
                    radius: 1500
                })
                .setDirection(feature.properties.antenna_direction, 45);
            } else if (feature.properties.tag === 'ClusterAntenna'){
                return L.circle(latlng, feature.properties.radius/ 5)
            } else {
              return  L.circleMarker(latlng, geojsonMarkerOptions)
            }
        }
    });
    m.addLayer(geoJsonLayer);
    var drawnItems = new L.FeatureGroup();
    m.addLayer(drawnItems);
    var drawControl = new L.Control.Draw({
        draw: {
            circle: false,
            circlemarker: false,
            polyline: false,
            marker: false,
            polygon: {
                allowIntersection: false,
                drawError: {
                    color: '#e1e100',
                    message: '<strong>Oh snap!<strong> you can\'t draw that!'
                },
                shapeOptions: {
                    color: '#00ff55'
                }
            },
            rectangle: false
        },
        edit: {
            featureGroup: drawnItems,
            poly: {
                allowIntersection: false,
            },
        }
    });
    m.addControl(drawControl);
    m.on(L.Draw.Event.CREATED, function (e) {
        var layer = e.layer;
        drawnItems.addLayer(layer);
    });
    
    m.on(L.Draw.Event.CREATED + " " +  L.Draw.Event.EDITED, (e) =>{
        drawnItems.eachLayer((l) => {
            onPolygonUpdate(l.toGeoJSON().geometry)()
            console.log("polygon created");
        });
        
    });
    m.on(L.Draw.Event.DRAWSTART + " " + L.Draw.Event.DELETED, (e) => {
        drawnItems.clearLayers();
        onPolygonDelete();
        console.log("polygon removed");
    });
    var updateGeoJSON = (geoJson) => {
        geoJsonLayer.clearLayers();
        geoJsonLayer.addData(geoJson);
    }

    var addGeoJSON = (geoJson) => {
        geoJsonLayer.addData(geoJson);
    }
    
    var fitBounds = () => {
        var boudns = geoJsonLayer.getBounds();
        m.fitBounds(boudns);
    }
    return {map: m, drawnItems, updateGeoJSON, getZoom: m.getZoom, getBounds: m.getBounds, fitBounds, addGeoJSON};
};

export const updateGeoJSON =  map => geoJson => () => {
    map.updateGeoJSON(geoJson);
}; 

export const addGeoJSON =  map => geoJson => () => {
    map.addGeoJSON(geoJson);
};

export const getZoom = map => () => {
    return map.getZoom();
}; 

export const getBounds =  map => () => {
    return map.getBounds();
}; 

export const fitBounds =  map => () => {
    return map.fitBounds();
};