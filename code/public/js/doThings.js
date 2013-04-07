$(document).ready(function() {

    var currentPage = $(".sidebar-nav a[href='#']").asEventStream('click').map(function(ev) {
        return $(ev.currentTarget).attr('data-target');
    }).toProperty('students').skipDuplicates();

    // Initialize (students) datatables with constant data
    var modalTemplate = $("#studentModalTemplate").text();
    var dstudents = $("#databox").dataTable( {
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
           dstudents.fnClearTable();
           dstudents.fnAddData(data);
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
           return dstudents.fnGetData(ev.currentTarget);
       });
   var studentQueries = dataClicks.flatMap(function(data) {
       return Bacon.fromPromise($.get("/student", {studentId: data[0]}));
   }).map(function(json) {
       return Mustache.render(modalTemplate, json);
   }).toProperty("");
   var studentModalClose = $("#modal_student a.close").asEventStream("click").map(false);
   var studentModalOpen = dataClicks.map(true);


   studentModalClose.merge(studentModalOpen).skipDuplicates().onValue(function(open) {
       console.log(open);
       if(open)
           $("#modal_student").modal('show');
       else
           $("#modal_student").modal('hide');
   });

   studentQueries.assign($('#studentBody'), 'html');
});
