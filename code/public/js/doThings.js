// When the page has been loaded
$(document).ready(function() {
    
    var currentPage = $(".sidebar-nav a[href='#']").asEventStream('click').map(function(ev) {
        return $(ev.currentTarget).attr('data-target');
    }).toProperty('students').skipDuplicates();
    currentPage.onValue(function(page) {
        // Sorry, not the most elegant solution. good enough when there are 3 pages
        if(page == 'students')
            $("#userData").show();
        else
            $("#userData").hide();
        if(page == 'degrees')
            $("#degreeData").show();
        else
            $("#degreeData").hide();
        if(page == 'credits')
            $("#courseData").show();
        else
            $("#courseData").hide();
    });

    // Reload the datatable based on what data is currently selected for viewing
    var initPage = function(settings) {
        var modalTemplate = settings.template
        var dtable = settings.dtElem.dataTable( {
            "bJQueryUI": true,
            "aoColumns": settings.dtColumns
        });

        var touch = function() {
            $.get(settings.dataUri, function(data) {
                dtable.fnClearTable();
                dtable.fnAddData(data);
            });
        };

        // Event stream for clicking on data in the datatable
        // The "click" type event for "tr" elements
        var dataClickStream = settings.dtElem.
            asEventStream("click", "tr").
            filter(function(ev) {
            var trg = $(ev.currentTarget);
            // In a datatable there are cells which should not trigger the event
            // The data fields have "odd" or "even" attributes while the headers and filters don't
            // By limiting our event to these two attributes we can make sure only the data triggers the event
            if(trg.attr("class") == "odd" || trg.attr("class") == "even")
                return true;
            return false;
        }).map(function(ev) {
            return dtable.fnGetData(ev.currentTarget);
        });
        // Based on our currently active page (students, degrees, credits),
        // get the appropriate "moreInfo" URI and get further details about the selected item
        // The URI is, for example, /students with the clicked tr cell's content being the parameter
        var templateProperty = dataClickStream.flatMap(function(data) {
            return Bacon.fromPromise(settings.moreInfo(data));
        }).map(function(json) {
            // Use Mustache.js to render the data into HTML based on a modalTemplate which
            // exists in the main HTML itself
            return Mustache.render(modalTemplate, json);
        }).toProperty("");

        // Events for closing and opening the modal which displays further details
        var modalClose = $("#modal a.close").asEventStream("click").map(false);
        var modalOpen = dataClickStream.map(true);

        modalClose.merge(modalOpen).skipDuplicates().onValue(function(open) {
            if(open)
                $("#modal").modal('show');
            else
                $("#modal").modal('hide');
        });

        // Write the final HTML in the #modalBody div
        templateProperty.assign($('#modalBody'), 'html');
        touch();

        return touch;
    }

    // Functions for setting up the main datatable based on which link in the
    // control panel was clicked. In case of students, format the datatable to
    // display student data. Also change the URI which is used for making queries about
    // the selected data
    window.touchStudents = initPage({
        template: $("#studentModalTemplate").text(),
        dtElem: $("#userData .databox"),
        dtColumns: [
            { "sTitle": "Student number" },
            { "sTitle": "Name" },
            { "sTitle": "Degree" },
            { "sTitle": "Major" },
            { "sTitle": "Student points"},
            { "sTitle": "Enrollment date"}
        ],
        moreInfo: function(data) { return $.get("/student", {studentId: data[0]}); },
        dataUri: "/student/data"
    });

    window.touchDegree = initPage({
        template: $("#degreeModalTemplate").text(),
        dtElem: $("#degreeData .databox"),
        dtColumns: [
            { "sTitle": "Degree" },
            { "sTitle": "# of known courses" },
        ],
        moreInfo: function(data) { return $.get("/thesis", {thesisId: data[0]}); },
        dataUri: "/degree/data"
    });

    window.touchCourse = initPage({
        template: $("#degreeModalTemplate").text(),
        dtElem: $("#courseData .databox"),
        dtColumns: [
            { "sTitle": "Course id" },
            { "sTitle": "Course name" },
            { "sTitle": "Course credits" },
        ],
        moreInfo: function(data) { return $.get("/course", {courseId: data[0]}); },
        dataUri: "/course/data"
    });

});

