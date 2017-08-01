
$(document).ready(function(){
    mapboxgl.accessToken = 'pk.eyJ1IjoiZ29ibXgxNSIsImEiOiJjaWZpaTl2eGtibDBjcnNtN3NqdW1wN25xIn0.yxAJmrmgXO_IaXuI1lckYA';
    var map = new mapboxgl.Map({
        container: 'map',
        style: 'mapbox://styles/mapbox/light-v9',
        center: [-99.177081, 19.401782],
        zoom: 12
    });
})


$('form').submit(function(e){
    var form = $(this).serializeArray();
    form[0].value
    // var win = window.open('/place/' + form[0].value + '/' + form[1].value + '/' + form[3].value + '/' + form[4].value + '/query.json');
    // win.focus();
    $.get('/place/' + form[0].value + '/' + form[1].value + '/' + form[3].value + '/' + form[4].value + '/query.json').done(function (data) {
        mapboxgl.accessToken = 'pk.eyJ1IjoiZ29ibXgxNSIsImEiOiJjaWZpaTl2eGtibDBjcnNtN3NqdW1wN25xIn0.yxAJmrmgXO_IaXuI1lckYA';
        var map = new mapboxgl.Map({
            container: 'map',
            style: 'mapbox://styles/mapbox/light-v9',
            center: [form[1].value, form.value[0]],
            zoom: 12
        });

        map.addSource('places', {
            'type':'geojson',
            'data': data
        })

        map.on('load', function(){
            map.addlayer({
                'id': 'places',
                'type': 'point',
                'source': 'places'
            })
        })

    });
    // e.preventDefault();
})