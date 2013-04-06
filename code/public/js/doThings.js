$(document).ready(function() {

    var active_mode = "students";

    $("#databox").html('<table cellpadding="0" cellspacing="0" border="0" class="display" id="outputdata"></table>');
    var dtable = $("#outputdata").dataTable( { 
        "aoColumns": [
        { "sTitle": "studentnumber" },
        { "sTitle": "name" },
        { "sTitle": "degree" },
        { "sTitle": "major" },
        { "sTitle": "points"},
        { "sTitle": "date"}
        ]
   });

    // Inserts the <th> tags required for listing students in the main databox
    function writeStudentHeaders() {
        // Call Happstack and ask blaze-html to nicely give us a new table with appropriate <th> tags
    }

    // Inserts the <th> tags required for listing degrees in the main databox
    function writeDegreeHeaders() {
        // Like above
    }

    function writeCourseHeaders() {
        // Like above
    }

    $('body').on('click', '#outputdata tbody tr td', function() {
        var tr = $(this).closest("tr");
        var number = tr.find("td:first").text();
        var name = tr.find("td:nth-child(2)").text();
        $("#infoModalHeader").html("Student");
        $("#infoModalBody").html("<p>" + number + ": " + name + "</p>");
        $("#infoModalFooter").html('"<button class="btn" data-dismiss="modal" aria-hidden="true">Close</button>"');
        $("#infoModal").modal({
            show: true
        });
    });

    $("#form_submit").click(function() {

        //var $form = $("#coolform");
        //var $inputs = $form.find("input", "text", "checkbox");
        //var serializedData = $form.serialize();

        //$inputs.prop("disabled", true);

        //console.log(serializedData);

        console.log(active_mode);

        var data_for_post;
        var url_for_post = "";
        if (active_mode == "students") {
            url_for_post = "/students";
            data_for_post = {
                FirstName: "",
                LastName: "",
                Points: null,
                Degree: "",
                Date: {
                    AEQ: [2010, ""]
                },
                Major: ""
            };
        } else if (active_mode == "degrees") {
            url_for_post = "/degrees";
            data_for_post = {
            }; 
        } else if (active_mode == "courses") {
            url_for_post = "/courses";
            data_for_post = {
            };
        }

        var output = $.ajax({
            url: url_for_post,
            dataType: "json",
            type: "post",
            data: data_for_post,
            success: function(obj) {
                $("#databox").empty();

                $("#databox").html('<table cellpadding="0" cellspacing="0" border="0" class="display" id="outputdata"></table>');
                if (active_mode == "students") {
                    var dtable = $("#outputdata").dataTable( { 
                        "aoColumns": [
                            { "sTitle": "studentnumber" },
                            { "sTitle": "name" },
                            { "sTitle": "degree" },
                            { "sTitle": "major" },
                            { "sTitle": "points"},
                            { "sTitle": "date"}
                            ]
                    });

                    $.each(obj, function(i, item) {
                        //console.log(obj[i].name);
                        //var template = "<td>{{studentId}}</td><td>{{name}}</td><td>{{degree}}</td><td>{{major}}</td><td>{{date.first}}</td>";
                        //textData += "[ {{studentId}}, {{name}}, {{degree}}, {{major}}, {{date}} ]"
                        //var html = Mustache.to_html(template, obj[i]);
                        dtable.fnAddData([ obj[i].studentId, obj[i].name, obj[i].degree, obj[i].major, obj[i].date, obj[i].studentPoints]);
                    });
                } else if (active_mode == "degrees") {
                } else if (active_mode == "courses") {
                }
            },
            error: function() {alert("no go");}
        });

        //var json_object = JSON.parse(output);
        //console.log(json_object);
        
        return false;
    });

    // When the user wants to view degrees
    $("#mode_degrees").asEventStream("click").subscribe(function(event) {
        active_mode = "degrees";
    });

    // When the user wants to view students
    $("#mode_students").asEventStream("click").subscribe(function(event) {
        active_mode = "students";
    });

    // When user wants to view courses
    $("#mode_courses").asEventStream("click").subscribe(function(event) {
        active_mode = "courses";
    });

    $("#outputdata tr").asEventStream("click").subscribe(function(event) {
        $(this).find("td").each(function(cellIndex) {
            console.log($(this).text());
        });
    });

    $("#student_link").asEventStream("click").subscribe(function(event) {
        // When a student is clicked, the data should change to display the records of this student
        // Moar ajax, get data, then
        // data = getStudentData($(this).val())
        // $("#datatable").clear();
        // $("#datatable").append(data);
    })

    $("#degree_link").asEventStream("click").subscribe(function(event) {
        // When a degree item is clicked, the data should change to show the courses
        // required to earn that degree (and other stuff?)
        // data = getDegreeData($(this).val())
        // $("#datatable").clear();
        // $("#datatable").append(data);
    })

    $("course_link").asEventStream("click").subscribe(function(event) {
        // When a course is clicked, the view should change to list the degrees to which
        // the course belongs to
        // data = getCourseData($(this).val())
        // $("#datatable").clear();
        // $("#datatable").append(data);
    });

    $("#clikme").asEventStream("click").subscribe(function(event) {
        alert("mmmm... bacon!")
    })

    function always(value) { return function() { return value }}
        
    function keyCodeIs(keyCode) { 
        return function(event) { return event.keyCode == keyCode }
    }

    function keyDowns(keyCode) { 
        return $(document).asEventStream("keydown").filter(keyCodeIs(keyCode))
    }

    function keyUps(keyCode) { 
        return $(document).asEventStream("keyup").filter(keyCodeIs(keyCode))
    }

    function keyState(keyCode) { 
        return keyDowns(keyCode).map(always("DOWN")).merge(keyUps(keyCode).map(always("UP"))).toProperty("UP")
    }

    keyState(32).onValue(function(state) {
        $("#state").text(state)
    })

    function isEmpty(s) { return s.length == 0 }
    
    //$("#controlpanel input").asEventStream("keyup").map(function(event) { return $(event.target).val()}).toProperty("").map(isEmpty).assign($("#controlpanel button"), "attr", "disabled") 

});

