
$('form').submit(function(e){
    $.post('/place', $(this).serializeArray()).done(function (data) {
        alert(data.message);
        if (data.status == 'Ok') {
            modal.modal('hide');
        }
    });
    e.preventDefault();
})