$(document).ready(function() {

    $("a").click(function() {
       alert("Handler for .click() called."); 
    });

    $("#coolform_submit").click(function() {
        var filterstring = $("#filterstring").val();
        var fname = $("#firstName").val();
        var lname = $("#lastName").val();
        var number = $("#number").val();
        var degree = $("#degree").val();
        var points = $("#points").val();
        var major = $("#major").val();

        $.ajax({
            url: "some_frp_call_thingy",
            type: "POST",
            data: "filterstring=" + filterstring + "&fname=" + fname + "&lname=" + lname + "&number=" + number + "&degree=" + degree + "&points=" + points + "&major=" + major,
            success: function(result) {
                console.log(result);
            }
        });
        
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
    
    $("#controlpanel input").asEventStream("keyup").map(function(event) { return $(event.target).val()}).toProperty("").map(isEmpty).assign($("#controlpanel button"), "attr", "disabled") 

});

