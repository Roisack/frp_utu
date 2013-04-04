$(document).ready(function() {

    $("#nav_box").hide();

    // Inserts the <th> tags required for listing students in the main databox
    function writeStudentHeaders() {
        // Call Happstack and ask blaze-html to nicely give us a new table with appropriate <th> tags
    }

    // Inserts the <th> tags required for listing degrees in the main databox
    function writeDegreeHeaders() {
        // Like above
    }

    $("#form_submit").click(function() {
        //if (request) {
        //    request.abort();
        //}

        //var $form = $("#coolform");
        //var $inputs = $form.find("input", "text", "checkbox");
        //var serializedData = $form.serialize();

        //$inputs.prop("disabled", true);

        var data1;
        data1 += $("#filterstring").val();
        data1 += $("#firstname").val();
        data1 += $("#lastname").val();
        console.log(data1);

        var response;
        //console.log(serializedData);

        $.ajax({
            url: "/students",
            type: "post",
            data: data1,
            success: function() {alert("success");},
            error: function() {alert("no go");}
        });
    });

    // When the user wants to view degrees
    $("#mode_degrees").asEventStream("click").subscribe(function(event) {
        writeDegreeHeaders();
    });

    // When the user wants to view students
    $("#mode_students").asEventStream("click").subscribe(function(event) {
        writeStudentHeaders();
    });

    // When user wants to view courses
    $("#mode_courses").asEventStream("click").subscribe(function(event) {
        writeCourseHeaders();
    });

    $("#sortbyfname").asEventStream("click").subscribe(function(event) {
        // When clicked, data should be sorted according to this setting
        // When clicked again, change sorting order from ascending to descenting
        // Possibly by an ajax call, reload all data and ask Haskell to output it in the correct form?
        // All the while the filter text should be remembered, so no extra names should be added
    })

    $("#sortbylname").asEventStream("click").subscribe(function(event) {
    })

    $("#sortbynumber").asEventStream("click").subscribe(function(event) {
    })

    $("#sortbydegree").asEventStream("click").subscribe(function(event) {
    })

    $("#sortbymajor").asEventStream("click").subscribe(function(event) {
    })

    $("#sortbypoints").asEventStream("click").subscribe(function(event) {
    })

    $("#sortbyx").asEventStream("click").subscribe(function(event) {
    })

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

