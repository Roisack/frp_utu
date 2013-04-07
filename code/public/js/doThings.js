$(document).ready(function() {

    var currentPage = $(".sidebar-nav a[href='#']").asEventStream('click').map(function(ev) {
        return $(ev.currentTarget).attr('data-target');
    }).toProperty('students').skipDuplicates();

    // Initialize (users) datatables with constant data
    var modalTemplate = $("#userModalTemplate").text();
    var dusers = $("#databox").dataTable( { 
        "bJQueryUI": true,
        "aaData" : studentData,
        "aoColumns": [
            { "sTitle": "Student number" },
            { "sTitle": "Name" },
            { "sTitle": "Degree" },
            { "sTitle": "Major" },
            { "sTitle": "Student points"},
            { "sTitle": "Enrollment date"}
        ]
   });
   window.touchStudents = function() {
       $.get("/student/data", function(data) {
           dusers.fnClearTable();
           dusers.fnAddData(data);
       });
   };

   var dataClicks = $("#databox").
       asEventStream("click", "tr").
       filter(function(ev) {
           var trg = $(ev.currentTarget);
           if(trg.attr("class") == "odd" || trg.attr("class") == "even")
               return true;
           return false;
       }).map(function(ev) {
           return dusers.fnGetData(ev.currentTarget);
       });
   var userQueries = dataClicks.flatMap(function(data) {
       return Bacon.fromPromise($.get("/user", {userId: data[0]}));
   }).map(function(json) {
       return Mustache.render(modalTemplate, json);
   }).toProperty("");
   var userModalClose = $("#modal_user a.close").asEventStream("click").map(false);
   var userModalOpen = dataClicks.map(true);


   userModalClose.merge(userModalOpen).skipDuplicates().onValue(function(open) {
       console.log(open);
       if(open)
           $("#modal_user").modal('show');
       else
           $("#modal_user").modal('hide');
   });

   userQueries.assign($('#userBody'), 'html');
});
