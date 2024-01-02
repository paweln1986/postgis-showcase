import * as leaflet from 'leaflet';
import 'leaflet-draw';
import 'leaflet-semicircle';

export const map = (cssClass) => (onPolygonUpdate) => (onPolygonDelete) => () => {
    let m = L.map(cssClass, {
        center: [51.06, 17.01],
        zoom: 10
    })
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        minZoom: 3,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(m);

    var drawnItems = new L.FeatureGroup();
    m.addLayer(drawnItems);
    L.semiCircle([51.06, 17.01], {radius: 5000})
        .setDirection(-90, -90)
        .addTo(drawnItems);
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

    return {map: m, drawnItems};
};
