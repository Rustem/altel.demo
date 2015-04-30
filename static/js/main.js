// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".watch-kpi", function(e) {
  e.preventDefault();
  $el = $(this);
  var bsID = $el.data("targetId");
  $($('#nav a[data-value="kpi"')).tab('show');
  console.log($($('#nav a[data-value="kpi"')), 'fasjdkfjasdkljflksdj')
  Shiny.onInputChange("gotoKPI", {
    id: bsID,
    nonce: Math.random()
  });
});

$(document).on('click', '.go-map', function(e) {
  e.preventDefault();
  $el = $(this);
  var bsID = $el.data('id');
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange('goto', {
    id: bsID,
    nonce: Math.random()
  })
});
