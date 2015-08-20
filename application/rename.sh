rename_plan()
{
sed -n -e '
h
s/application/Application/
s/asset/Asset/
s/component/Component/
x
G
s/\n/|/
p
' <<EOF
gasoline_Generic_application.ml
gasoline_Generic_application.mli
gasoline_Generic_asset.ml
gasoline_Generic_asset.mli
gasoline_Generic_component.ml
gasoline_Generic_component.mli
EOF
}

rename_plan | while IFS='|' read old new; do
    git mv "${old}" "${new}"
    oldpat="${old%.*}"
    oldpat="${oldpat#g}"
    newpat="${new%.*}"
    newpat="${newpat#g}"
    sed -i '' -e "s/${oldpat}/${newpat}/g" Makefile *.ml *.mli
done
