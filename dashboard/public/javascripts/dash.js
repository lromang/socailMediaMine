
$('form').submit(function(e){
    var form = $(this).serializeArray();
    form[0].value
    var win = window.open('/place/' + form[0].value + '/' + form[1].value + '/' + form[3].value + '/' + form[4].value + '/query.json');
    win.focus();
    /*$.get('/place/' + form[0].value + '/' + form[1].value + '/' + form[3].value + '/' + form[4].value + '/query.json').done(function (data) {

    });*/
    // e.preventDefault();
})