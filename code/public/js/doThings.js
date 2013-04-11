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
            $("#creditData").show();
        else
            $("#creditData").hide();
    });

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

        var dataClickStream = settings.dtElem.
            asEventStream("click", "tr").
            filter(function(ev) {
            var trg = $(ev.currentTarget);
            if(trg.attr("class") == "odd" || trg.attr("class") == "even")
                return true;
            return false;
        }).map(function(ev) {
            return dtable.fnGetData(ev.currentTarget);
        });
        var templateProperty = dataClickStream.flatMap(function(data) {
            return Bacon.fromPromise(settings.moreInfo(data));
        }).map(function(json) {
            return Mustache.render(modalTemplate, json);
        }).toProperty("");
        var modalClose = $("#modal a.close").asEventStream("click").map(false);
        var modalOpen = dataClickStream.map(true);


        modalClose.merge(modalOpen).skipDuplicates().onValue(function(open) {
            if(open)
                $("#modal").modal('show');
            else
                $("#modal").modal('hide');
        });

        templateProperty.assign($('#modalBody'), 'html');
        touch();

        return touch;
    }

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
        moreInfo: function(data) { return $.get("/degree", {studentId: data[0]}); },
        dataUri: "/degree/data"
    });

});

