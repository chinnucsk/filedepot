$(document).ready(function() {

	$("#files tbody tr").click(function() {

		if ($(this).next().hasClass("preview")) {
			$(this).next().toggle();
		}
		else {
			var target = $(this);
			var url = $(this).find('a').attr('href');
			// Fetch the file contents from the given url
			$.get(url, function(ret) {

			}).complete(function(jqXHR, textStatus) {
				// When request has completed,
				// let's display the contents depending on the content-type
				var contentType = jqXHR.getResponseHeader('Content-Type');
				var data = jqXHR.responseText;
				// If it's an image, display content in an <img> tag
				if (contentType.split('/')[0] == "image") {
					var img = '<img src="' + url + '" />';
					target.after('<tr class="preview"><td colspan="3">' + img + '</td></tr>');
				}
				// Otherwise, print contents
				else {
					switch (contentType) {
						case "text/plain":
							output = data;
							break;
						default:
							output = "Preview for this filetype not supported yet. Click the filename to preview in a new browser tab.";
							break;
					}
					// Make sure newlines are printed
					output = output.replace(/\n/g, "<br>");
					// Insert the data
					target.after('<tr class="preview"><td colspan="3">' + output + '</td></tr>');
				}
			});
		}
	}); // End #files click

	var dropbox = $('#dropbox'),
			message = $('.message', dropbox);

	dropbox.filedrop({
		// The name of the files array
		paramname: 'files',

		maxfiles: 5,
		maxfilesize: 5, // in megabytes
		url: dropbox.attr('data-target'),

		// Set a lighter border-color when we drag a file over the dropbox
		dragOver: function() {
			dropbox.css('border-color', '#ccc');
		},

		// Return to normal after we leave the dropbox
		dragLeave: function() {
			dropbox.css('border-color', '#000');
		},

		// file is a File object containing
		// 	lastModifiedDate, name, size, webkitRelativePath
		// response is the path to the temporarily stored file

		uploadFinished: function(i, file, response) {
			// If the save button isn't visible, show it
			if ($("#saveButton").hasClass('hidden'))
				$("#saveButton").show();
			// Store the temporary dir
			// console.log(file);
			// 			console.log(response);
		},

		error: function(err, file) {
			switch (err) {
				case 'BrowserNotSupported':
					showMessage('Your browser does not support HTML5 file uploads!');
					break;
				case 'TooManyFiles':
					showWarning('Too many files! Please select 5 at most!');
					break;
				case 'FileTooLarge':
					showWarning('\'' + file.name + '\' is too large! Max size allowed 5 MB');
					break;
				default:
					break;
			}
		},

		// Called before each upload is started
		beforeEach: function(file){
			// console.log("Before!");
			// SHOW SPINNER
			// if(!file.type.match(/^image\//)){
			// 				alert('Only images are allowed!');
			//
			// 				// Returning false will cause the
			// 				// file to be rejected
			// 				return false;
			// 			}
		},

		uploadStarted:function(i, file, len){
			var preview = previewFile(file.name, file.size);
			// preview[0] because an array of objects is returned from previewFile() but there is only one element
			// Bind the file data to this particular preview element
			$.data(preview[0], file);
		},

		progressUpdated: function(i, file, progress) {
			updateProgressBar(file.name, progress);
			// $.data(file).find('.progress').width(progress);
		},

		speedUpdated: function(i, file, speed) {
			console.log(speed + " kb/s");
    },
	});

	// After we've uploaded our files, we want to save them to the server
	$("#saveButton").click(function() {
		$("#saveAlert").html("Files saved!");
		$("#saveAlert").show();

		var files = $("#preview tbody tr");
		console.log("SaveButton: ");
		console.log(files);
		console.log($.data(files[0]));
		$.each(files, function(i, file) {

			console.log($.data(file));
		});
	});

	// Remove one of the preview files
	$(document).on("click", ".delete", function(event) {
		console.log($(this).parent().parent());
	});

	function showWarning(msg) {
		message.addClass('warning');
		message.html(msg);
	}

	function showMessage(msg){
		message.html(msg);
	}

	// This function updates the progress bar belonging to 'filename' to 'progress'
	function updateProgressBar(filename, progress) {
		// Apply a filter only to keep the one row that contains 'filename'
		var tr = $("#preview tbody tr").filter(function(i) {
			return $('td.filename', this).html() == filename;
		});
		// Set the progress bar to 'progress' %
		$('.progress .bar', tr).css('width', progress+'%');
		// Remove the 'active' animation if the progress is updated to 100%
		if (progress == 100)
			$('.progress', tr).removeClass('active');
	}

	function previewFile(filename, size) {
		var target = $("#preview");
		var tbody = target.find("tbody");

		// Structure for the progress bar
		var progress =
		'<div class="progress progress-striped active">' +
  		'<div class="bar" style="width: 0%;"></div>' +
		'</div>'

		// Structure for the addition/removal of tags
		var tags = '<a href="#">edit tags</a>';

		// Structure for the remove file link
		var removal = '<a class="delete" href="#">X</a>';
		//'<button class="btn btn-mini btn-danger">Delete</button>';
		//'<a href="#"><i class="icon-remove-sign"></i></a>';

		// Everything put together
		var template =
			'<td class="filename">' + filename + '</td>' +
			'<td>' + fileSizeFormat(size) +'</td>' +
			'<td>' + progress +'</td>' +
			'<td>' + tags +'</td>' +
			'<td>' + removal +'</td>';
		// Add this to a table row
		var tr = $(document.createElement('tr')).append(template);
		// Add the row to the preview table's tbody
		tbody.append(tr);
		// Return it so we can bind some File data to it
		return tr;
	}

	// Format the value like a human-readable file size
	var FD_KILOBYTE = 1024;
	var FD_MEGABYTE = FD_KILOBYTE * 1024;
	var FD_GIGABYTE = FD_MEGABYTE * 1024;

	function fileSizeFormat(size) {
		if (size >= FD_GIGABYTE)
			return fileSizeFormat(size/FD_GIGABYTE) + " GB";
		else if (size >= FD_MEGABYTE)
			return fileSizeFormat(size/FD_MEGABYTE) + " MB";
		else if (size >= FD_KILOBYTE)
			return fileSizeFormat(size/FD_KILOBYTE) + " KB";
		else if (size.toString().indexOf(".") != -1)
			return Math.round(size*100)/100;
		else
			return size + " bytes";
	}

});