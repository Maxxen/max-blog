
function toggleResponsiveNav() {
    var elem = document.getElementById("navigation");
    if (elem.className === "menu") {
        elem.className += " responsive";
    } else {
        elem.className = "menu";
    }
}
