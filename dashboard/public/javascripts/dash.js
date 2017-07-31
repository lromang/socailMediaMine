
$('form').submit(function(e){
    $.get('/place.json', $(this).serializeArray()).done(function (data) {
        alert(data.message);
        if (data.status == 'Ok') {
            modal.modal('hide');
        }
    });
    e.preventDefault();
})