$(document).ready(function() {
    // Define the custom message handler once
    Shiny.addCustomMessageHandler("dir_selected", function(response) {

        if (response.toString() != "") {
            let imageList = document.getElementById('image_list').
              getElementsByTagName('div');


            for (let item of imageList) {}
        }

    });

});


function changeBackground(element) {
    // Remove 'selected' class from all image divs
    const imageDivs = document.getElementsByClassName('imlist');
    console.log(imageDivs)
    for (let div of imageDivs) {
        div.classList.remove('selected');
    }

    // Add 'selected' class to the clicked image div
    element.classList.add('selected');
}
