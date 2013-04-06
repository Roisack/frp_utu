$(document).ready(function() {

    $("#databox").dataTable( { 
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
});
