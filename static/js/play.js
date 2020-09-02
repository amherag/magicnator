function viewCard(card) {
    if (card == null) {
	$("#card-viewer").html("");
    } else {
	$("#card-viewer").html($('<img>').attr("src", card.src).attr("alt", card.alt).attr("height", "600px"));
    }
}

function clickCard(e) {
    e = e || window.event;
    var target = e.target || e.srcElement;
    

    // if (e.altKey) {
    // 	$(target).parent().toggleClass('go-under');
    // 	return
    // }

    // if (e.shiftKey) {
    // 	$(target).parent().toggleClass('go-over');
    // 	return
    // }

    $(target).toggleClass('flip');
}

$(window).on("load", ()=>{
    // recenterHand()
    // dragula([$("#hand"), $("#creatures")])
    dragula([document.getElementById("hand-block"),
	     document.getElementById("lands-block"),
	     document.getElementById("creatures-block"),
	     document.getElementById("artifacts-block"),
	     document.getElementById("planeswalkers-block"),
	     document.getElementById("planeswalkers-block")])
})
