document.addEventListener('DOMContentLoaded', function() {
  document.getElementById('add_sh').addEventListener('click', function() {
    const inputField = document.getElementById('sh_set')
    const labels = inputField.getElementsByTagName('label')
    const options = inputField.getElementsByTagName('option')

});

});

document.addEventListener('DOMContentLoaded', function() {
  document.getElementById('apply_insertion').addEventListener('click', function() {

    const inputField = document.getElementById('hs_set')
    const labels = inputField.getElementsByTagName('label')
    const options = inputField.getElementsByTagName('option')

    let result = [];

    for (var i = 0; i < labels.length; i++) {
      const label = labels[i].innerText.trim();
      const value = options[i].innerText.trim();
      result.push({ label: label, value: value });
    }

    // Send the result to Shiny
    Shiny.setInputValue('js_geths', result);

  })

});
